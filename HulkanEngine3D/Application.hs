{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}

module HulkanEngine3D.Application
    ( InputData (..)
    , glfwMainLoop
    , createGLFWWindow
    , destroyGLFWWindow
    , charCallBack
    , runApplication
    ) where

import Control.Monad
import Data.IORef
import Data.Maybe (isNothing)
import qualified Data.DList as DList
import System.Directory
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import HulkanEngine3D.Resource.ObjLoader
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Vulkan
import HulkanEngine3D.Vulkan.Mesh
import HulkanEngine3D.Vulkan.Device
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.TransformationObject
import qualified HulkanEngine3D.Constants as Constants

data InputData = InputData
    {
    } deriving (Show)

data Application = Application
    { _renderTargetData :: RenderTargetData
    } deriving (Show)

createGLFWWindow::Int -> Int -> String -> IORef Bool -> IO (Maybe GLFW.Window)
createGLFWWindow width height title windowSizeChanged = do
    GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")
    logInfo "Initialized GLFW."
    version <- GLFW.getVersionString
    mapM_ (logInfo . ("GLFW Version: " ++)) version
    GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
    GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    GLFW.windowHint $ WindowHint'Resizable True
    maybeWindow <- GLFW.createWindow width height title Nothing Nothing
    let Just window = maybeWindow
    GLFW.setWindowSizeCallback window $
        Just (\_ _ _ -> atomicWriteIORef windowSizeChanged True)
    GLFW.setKeyCallback window $ Just keyCallBack
    GLFW.setCharCallback window $ Just charCallBack
    return maybeWindow

destroyGLFWWindow :: GLFW.Window -> IO ()
destroyGLFWWindow window = do
    GLFW.destroyWindow window >> logInfo "Closed GLFW window."
    GLFW.terminate >> logInfo "Terminated GLFW."


updateEvent :: IO ()
updateEvent = return ()


keyCallBack :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallBack window key scancode state mods = do
    logInfo $ show window
    logInfo $ show key
    logInfo $ show scancode
    logInfo $ show state
    logInfo $ show mods

charCallBack :: GLFW.Window -> Char -> IO ()
charCallBack windows key = do
    logInfo $ show key


glfwMainLoop :: GLFW.Window -> IO Bool -> IO ()
glfwMainLoop window mainLoop = go
  where
    go = do
      should <- GLFW.windowShouldClose window
      unless should $ do
        GLFW.pollEvents
        updateEvent
        result <- mainLoop
        if result then go else return ()

runApplication :: IO()
runApplication = do
    windowSizeChanged <- newIORef False
    maybeWindow <- createGLFWWindow 1024 768 "Vulkan Application" windowSizeChanged
    when (isNothing maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
    logInfo "                             "
    logInfo "<< Initialized GLFW window >>"
    requireExtensions <- GLFW.getRequiredInstanceExtensions
    instanceExtensionNames <- getInstanceExtensionSupport
    checkExtensionResult <- checkExtensionSupport instanceExtensionNames requireExtensions
    unless checkExtensionResult (throwVKMsg "Failed to initialize GLFW window.")
    let Just window = maybeWindow
        progName = "Hulkan App"
        engineName = "HulkanEngine3D"
        enableValidationLayer = True
        isConcurrentMode = True
        msaaSampleCount = VK_SAMPLE_COUNT_4_BIT
    -- create renderer
    defaultRendererData <- getDefaultRendererData
    rendererData <- createRenderer
        defaultRendererData
            window
            progName
            engineName
            enableValidationLayer
            isConcurrentMode
            requireExtensions
            msaaSampleCount
    rendererDataRef <- newIORef rendererData

    -- create render targets
    renderTargetData <- createRenderTargets rendererData
    sceneColorTextureRef <- newIORef (_sceneColorTexture renderTargetData)
    sceneDepthTextureRef <- newIORef (_sceneDepthTexture renderTargetData)

    -- create render pass data
    renderPassDataCreateInfo <- getDefaultRenderPassDataCreateInfo
        rendererData
        [(_imageView (_sceneColorTexture renderTargetData)), (_imageView (_sceneDepthTexture renderTargetData))]
        [getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0]
    renderPassData <- createRenderPass rendererData renderPassDataCreateInfo
    renderPassDataListRef <- newIORef (DList.singleton renderPassData)

    -- create resources
    (vertices, indices) <- loadModel "Resource/Externals/Meshes/suzan.obj"
    geometryBuffer <- createGeometryBuffer rendererData "test" vertices indices
    geometryBufferListRef <- newIORef (DList.singleton geometryBuffer)
    textureData <- createTexture rendererData "Resource/Externals/Textures/texture.jpg"

    (transformObjectMemories, transformObjectBuffers) <- unzip <$> createTransformObjectBuffers
        (getPhysicalDevice rendererData)
        (getDevice rendererData)
        (getSwapChainImageCount rendererData)
    descriptorPool <- createDescriptorPool (getDevice rendererData) (getSwapChainImageCount rendererData)
    descriptorSetData <- createDescriptorSetData (getDevice rendererData) descriptorPool (getSwapChainImageCount rendererData) (getDescriptorSetLayout renderPassData)
    let descriptorBufferInfos = fmap transformObjectBufferInfo transformObjectBuffers
    forM_ (zip descriptorBufferInfos (_descriptorSets descriptorSetData)) $ \(descriptorBufferInfo, descriptorSet) ->
        prepareDescriptorSet (getDevice rendererData) descriptorBufferInfo (getTextureImageInfo textureData) descriptorSet

    -- record render commands
    let vertexBuffer = _vertexBuffer geometryBuffer
        vertexIndexCount = _vertexIndexCount geometryBuffer
        indexBuffer = _indexBuffer geometryBuffer
    recordCommandBuffer rendererData renderPassData vertexBuffer (vertexIndexCount, indexBuffer) (_descriptorSets descriptorSetData)

    -- init system variables
    needRecreateSwapChainRef <- newIORef False
    frameIndexRef <- newIORef 0
    imageIndexPtr <- new (0 :: Word32)

    currentTime <- getSystemTime
    currentTimeRef <- newIORef currentTime
    elapsedTimeRef <- newIORef (0.0 :: Double)

    -- Main Loop
    glfwMainLoop window $ do
        currentTime <- getSystemTime
        previousTime <- readIORef currentTimeRef
        let deltaTime = currentTime - previousTime
        elapsedTime <- do
            elapsedTimePrev <- readIORef elapsedTimeRef
            return $ elapsedTimePrev + deltaTime
        writeIORef currentTimeRef currentTime
        writeIORef elapsedTimeRef elapsedTime
        -- when (0.0 < deltaTime) . logInfo $ show (1.0 / deltaTime) ++ "fps / " ++ show deltaTime ++ "ms"

        needRecreateSwapChain <- readIORef needRecreateSwapChainRef
        when needRecreateSwapChain $ do
            writeIORef needRecreateSwapChainRef False
            logInfo "                        "
            logInfo "<< Recreate SwapChain >>"

            -- cleanUp swapChain
            rendererData <- readIORef rendererDataRef
            result <- vkDeviceWaitIdle $ _device rendererData
            validationVK result "vkDeviceWaitIdle failed!"

            renderPassDataList <- readIORef renderPassDataListRef
            forM_ renderPassDataList $ \renderPassData -> do
                destroyRenderPass rendererData renderPassData

            sceneColorTexture <- readIORef sceneColorTextureRef
            sceneDepthTexture <- readIORef sceneDepthTextureRef
            destroyTexture rendererData sceneColorTexture
            destroyTexture rendererData sceneDepthTexture

            -- recreate swapChain
            rendererData <- recreateSwapChain rendererData window

            -- recreate resources
            renderTargetData <- createRenderTargets rendererData
            writeIORef sceneColorTextureRef (_sceneColorTexture renderTargetData)
            writeIORef sceneDepthTextureRef (_sceneDepthTexture renderTargetData)

            renderPassDataCreateInfo <- getDefaultRenderPassDataCreateInfo
                rendererData
                [(_imageView (_sceneColorTexture renderTargetData)), (_imageView (_sceneDepthTexture renderTargetData))]
                [getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0]
            renderPassData <- createRenderPassData (getDevice rendererData) renderPassDataCreateInfo

            -- record render commands
            let vertexBuffer = _vertexBuffer geometryBuffer
                vertexIndexCount = _vertexIndexCount geometryBuffer
                indexBuffer = _indexBuffer geometryBuffer
            recordCommandBuffer rendererData renderPassData vertexBuffer (vertexIndexCount, indexBuffer) (_descriptorSets descriptorSetData)

            writeIORef renderPassDataListRef (DList.fromList [renderPassData])
            writeIORef rendererDataRef rendererData
        frameIndex <- readIORef frameIndexRef
        rendererData <- readIORef rendererDataRef
        renderPassDataList <- readIORef renderPassDataListRef

        result <- drawFrame rendererData frameIndex imageIndexPtr transformObjectMemories
        vkDeviceWaitIdle (getDevice rendererData)
        writeIORef frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
        sizeChanged <- readIORef windowSizeChanged
        when (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged) $ do
            atomicWriteIORef windowSizeChanged False
            writeIORef needRecreateSwapChainRef True
        return True

    -- Terminate
    logInfo "               "
    logInfo "<< Terminate >>"
    rendererData <- readIORef rendererDataRef
    result <- vkDeviceWaitIdle $ _device rendererData
    validationVK result "vkDeviceWaitIdle failed!"

    destroyTransformObjectBuffers (getDevice rendererData) transformObjectBuffers transformObjectMemories

    -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for createDescriptorPool
    -- destroyDescriptorSetData (getDevice rendererData) descriptorPool descriptorSetData
    destroyDescriptorPool (getDevice rendererData) descriptorPool

    sceneColorTexture <- readIORef sceneColorTextureRef
    sceneDepthTexture <- readIORef sceneDepthTextureRef
    destroyTexture rendererData sceneColorTexture
    destroyTexture rendererData sceneDepthTexture

    destroyTexture rendererData textureData

    geometryBufferList <- readIORef geometryBufferListRef
    forM_ geometryBufferList $ \geometryBuffer -> do
        destroyGeometryBuffer rendererData geometryBuffer

    renderPassDataList <- readIORef renderPassDataListRef
    forM_ renderPassDataList $ \renderPassData -> do
        destroyRenderPassData (_device rendererData) renderPassData

    destroyRenderer rendererData
    free imageIndexPtr

    destroyGLFWWindow window

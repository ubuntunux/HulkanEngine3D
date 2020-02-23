{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}


module HulkanEngine3D.Application
    ( ApplicationData (..)
    , runApplication
    ) where

import Control.Monad
import Data.IORef
import Data.Maybe (isNothing)
import qualified Data.DList as DList
import System.Directory
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import qualified Control.Lens as Lens

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


data TestData = TestData
    { _list0 :: [Integer]
    , _list1 :: [Integer]
    , _list2 :: [Integer]
    } deriving (Show)

data TestDataRef = TestDataRef
    { _listRef0 :: IORef [Integer]
    , _listRef1 :: IORef [Integer]
    , _listRef2 :: IORef [Integer]
    } deriving (Show)

data InputData = InputData
    {
    } deriving (Show)

data ApplicationData = ApplicationData
    { _window :: GLFW.Window
    , _needRecreateSwapChainRef :: IORef Bool
    , _windowSizeChanged :: IORef Bool
    , _frameIndexRef  :: IORef Int
    , _imageIndexPtr :: Ptr Word32
    , _currentTime :: Double
    , _elapsedTime :: Double
    , _renderTargetData :: RenderTargetData
    , _rendererDataRef :: IORef RendererData
    , _renderPassDataListRef :: IORef (DList.DList RenderPassData)
    , _transformObjectBuffers :: [VkBuffer]
    , _transformObjectMemories :: [VkDeviceMemory]
    , _descriptorPool :: VkDescriptorPool
    , _descriptorSetData :: DescriptorSetData
    , _textureData :: TextureData
    , _geometryBuffer :: GeometryBufferData
    } deriving (Show)

Lens.makeLenses ''ApplicationData


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


initializeApplication :: IO ApplicationData
initializeApplication = do
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

    let x = [0..20000]
        y = [9..20009]
        z = [3..20003]

    let testData = TestData
            { _list0 = x
            , _list1 = y
            , _list2 = z
            }

    let testDataRef = TestDataRef
            { _listRef0 = newIORef x
            , _listRef1 = newIORef y
            , _listRef2 = newIORef z
            }


    return ApplicationData
            { _window = window
            , _needRecreateSwapChainRef = needRecreateSwapChainRef
            , _windowSizeChanged = windowSizeChanged
            , _frameIndexRef = frameIndexRef
            , _imageIndexPtr = imageIndexPtr
            , _currentTime = currentTime
            , _elapsedTime = 0.0
            , _renderTargetData = renderTargetData
            , _rendererDataRef = rendererDataRef
            , _renderPassDataListRef = renderPassDataListRef
            , _transformObjectBuffers = transformObjectBuffers
            , _transformObjectMemories = transformObjectMemories
            , _descriptorPool = descriptorPool
            , _descriptorSetData = descriptorSetData
            , _textureData = textureData
            , _geometryBuffer = geometryBuffer
            }

    where
        loop :: Int -> TestData -> TestData
        loop x testData = do
              if (x < 1000)
              then loop (x+1) testData
              else testData


updateLoop :: ApplicationData -> (ApplicationData -> IO ApplicationData) -> IO ApplicationData
updateLoop applicationData loopAction = do
    exit <- GLFW.windowShouldClose (_window applicationData)
    if not exit then do
        GLFW.pollEvents
        updateEvent
        newApplicationData <- loopAction applicationData
        updateLoop newApplicationData loopAction
    else
        return applicationData

terminateApplication :: ApplicationData -> IO ()
terminateApplication applicationData = do
    logInfo "               "
    logInfo "<< Terminate >>"

    rendererData <- readIORef (_rendererDataRef applicationData)

    -- waiting
    deviceWaitIdle rendererData

    destroyTransformObjectBuffers
        (getDevice rendererData)
        (_transformObjectBuffers applicationData)
        (_transformObjectMemories applicationData)

    -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for createDescriptorPool
    -- destroyDescriptorSetData (getDevice rendererData) descriptorPool descriptorSetData
    destroyDescriptorPool (getDevice rendererData) (_descriptorPool applicationData)

    destroyTexture rendererData (_sceneColorTexture (_renderTargetData applicationData))
    destroyTexture rendererData (_sceneDepthTexture (_renderTargetData applicationData))

    destroyTexture rendererData (_textureData applicationData)

    destroyGeometryBuffer rendererData (_geometryBuffer applicationData)

    renderPassDataList <- readIORef (_renderPassDataListRef applicationData)
    forM_ renderPassDataList $ \renderPassData -> do
        destroyRenderPassData (_device rendererData) renderPassData

    destroyRenderer rendererData
    free (_imageIndexPtr applicationData)

    destroyGLFWWindow (_window applicationData)

runApplication :: IO()
runApplication = do
    initializedApplicationData <- initializeApplication

    -- Main Loop
    finalApplicationData <- updateLoop initializedApplicationData $ \applicationData -> do
        currentTime <- getSystemTime
        let previousTime = (_currentTime applicationData)
            deltaTime = currentTime - previousTime
            elapsedTime = (_elapsedTime applicationData) + deltaTime
        when (0.0 < deltaTime) . logInfo $ show (1.0 / deltaTime) ++ "fps / " ++ show deltaTime ++ "ms"

        needRecreateSwapChain <- readIORef (_needRecreateSwapChainRef applicationData)
        renderTargetData <-
            if needRecreateSwapChain then do
                atomicWriteIORef (_needRecreateSwapChainRef applicationData) False
                logInfo "                        "
                logInfo "<< Recreate SwapChain >>"

                -- cleanUp swapChain
                rendererData <- readIORef (_rendererDataRef applicationData)

                -- waiting
                deviceWaitIdle rendererData

                renderPassDataList <- readIORef (_renderPassDataListRef applicationData)
                forM_ renderPassDataList $ \renderPassData -> do
                    destroyRenderPass rendererData renderPassData

                destroyTexture rendererData (_sceneColorTexture (_renderTargetData applicationData))
                destroyTexture rendererData (_sceneDepthTexture (_renderTargetData applicationData))

                -- recreate swapChain
                rendererData <- recreateSwapChain rendererData (_window applicationData)

                -- recreate resources
                renderTargetData <- createRenderTargets rendererData

                renderPassDataCreateInfo <- getDefaultRenderPassDataCreateInfo
                    rendererData
                    [(_imageView (_sceneColorTexture renderTargetData)), (_imageView (_sceneDepthTexture renderTargetData))]
                    [getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0]
                renderPassData <- createRenderPassData (getDevice rendererData) renderPassDataCreateInfo

                -- record render commands
                let vertexBuffer = _vertexBuffer (_geometryBuffer applicationData)
                    vertexIndexCount = _vertexIndexCount (_geometryBuffer applicationData)
                    indexBuffer = _indexBuffer (_geometryBuffer applicationData)
                    descriptorSetData = (_descriptorSetData applicationData)
                recordCommandBuffer rendererData renderPassData vertexBuffer (vertexIndexCount, indexBuffer) (_descriptorSets descriptorSetData)

                writeIORef (_renderPassDataListRef applicationData) (DList.fromList [renderPassData])
                writeIORef (_rendererDataRef applicationData) rendererData
                return renderTargetData
            else
                return (_renderTargetData applicationData)
        frameIndex <- readIORef (_frameIndexRef applicationData)
        rendererData <- readIORef (_rendererDataRef applicationData)
        renderPassDataList <- readIORef (_renderPassDataListRef applicationData)

        result <- drawFrame rendererData frameIndex (_imageIndexPtr applicationData) (_transformObjectMemories applicationData)

        -- waiting
        deviceWaitIdle rendererData

        writeIORef (_frameIndexRef applicationData) $ mod (frameIndex + 1) Constants.maxFrameCount
        sizeChanged <- readIORef (_windowSizeChanged applicationData)
        when (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged) $ do
            atomicWriteIORef (_windowSizeChanged applicationData) False
            atomicWriteIORef (_needRecreateSwapChainRef applicationData) True

        return applicationData
            { _renderTargetData = renderTargetData
            , _currentTime = currentTime
            , _elapsedTime = elapsedTime }

    terminateApplication finalApplicationData


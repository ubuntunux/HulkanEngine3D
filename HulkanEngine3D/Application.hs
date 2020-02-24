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
import qualified Data.HashTable.IO as HashTable
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
    { _keyboardDown :: Bool
    , _keyboardPressed :: Bool
    , _keyboardUp :: Bool
    , _keyPressed :: ()
    , _keyReleased :: ()
    } deriving (Show)

Lens.makeLenses ''InputData

data ApplicationData = ApplicationData
    { _window :: GLFW.Window
    , _windowSizeChanged :: IORef Bool
    , _needRecreateSwapChain :: Bool
    , _frameIndex  :: Int
    , _imageIndexPtr :: Ptr Word32
    , _currentTime :: Double
    , _elapsedTime :: Double
    , _inputData :: InputData
    , _renderTargetData :: RenderTargetData
    , _rendererData :: RendererData
    , _renderPassDataList :: (DList.DList RenderPassData)
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

    -- create render targets
    renderTargetData <- createRenderTargets rendererData

    -- create render pass data
    renderPassDataCreateInfo <- getDefaultRenderPassDataCreateInfo
        rendererData
        [(_imageView (_sceneColorTexture renderTargetData)), (_imageView (_sceneDepthTexture renderTargetData))]
        [getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0]
    renderPassData <- createRenderPass rendererData renderPassDataCreateInfo
    let renderPassDataList = (DList.singleton renderPassData)

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
    imageIndexPtr <- new (0 :: Word32)
    currentTime <- getSystemTime
    let inputData = InputData
            { _keyboardDown = False
            , _keyboardPressed = False
            , _keyboardUp = False
            , _keyPressed = ()
            , _keyReleased = ()
            }

    return ApplicationData
            { _window = window
            , _needRecreateSwapChain = False
            , _windowSizeChanged = windowSizeChanged
            , _frameIndex = 0
            , _imageIndexPtr = imageIndexPtr
            , _currentTime = currentTime
            , _elapsedTime = 0.0
            , _inputData = inputData
            , _renderTargetData = renderTargetData
            , _rendererData = rendererData
            , _renderPassDataList = renderPassDataList
            , _transformObjectBuffers = transformObjectBuffers
            , _transformObjectMemories = transformObjectMemories
            , _descriptorPool = descriptorPool
            , _descriptorSetData = descriptorSetData
            , _textureData = textureData
            , _geometryBuffer = geometryBuffer
            }

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

    let rendererData = (_rendererData applicationData)

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

    forM_ (_renderPassDataList applicationData) $ \renderPassData -> do
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
        --when (0.0 < deltaTime) . logInfo $ show (1.0 / deltaTime) ++ "fps / " ++ show deltaTime ++ "ms"

        let frameIndex = (_frameIndex applicationData)

        (renderTargetData, renderPassDataList, rendererData) <-
            if (_needRecreateSwapChain applicationData) then do
                logInfo "                        "
                logInfo "<< Recreate SwapChain >>"

                -- cleanUp swapChain
                let rendererData = (_rendererData applicationData)

                -- waiting
                deviceWaitIdle rendererData

                forM_ (_renderPassDataList applicationData) $ \renderPassData -> do
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
                let renderPassDataList = (DList.fromList [renderPassData])

                -- record render commands
                let vertexBuffer = _vertexBuffer (_geometryBuffer applicationData)
                    vertexIndexCount = _vertexIndexCount (_geometryBuffer applicationData)
                    indexBuffer = _indexBuffer (_geometryBuffer applicationData)
                    descriptorSetData = (_descriptorSetData applicationData)
                recordCommandBuffer rendererData renderPassData vertexBuffer (vertexIndexCount, indexBuffer) (_descriptorSets descriptorSetData)

                return (renderTargetData, renderPassDataList, rendererData)
            else
                return (_renderTargetData applicationData, _renderPassDataList applicationData, _rendererData applicationData)

        result <- drawFrame rendererData frameIndex (_imageIndexPtr applicationData) (_transformObjectMemories applicationData)

        -- waiting
        deviceWaitIdle rendererData

        sizeChanged <- readIORef (_windowSizeChanged applicationData)
        let needRecreateSwapChain = (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged)
        when needRecreateSwapChain $ atomicWriteIORef (_windowSizeChanged applicationData) False

        let nextFrameIndex = mod (frameIndex + 1) Constants.maxFrameCount

        return applicationData
            { _currentTime = currentTime
            , _elapsedTime = elapsedTime
            , _needRecreateSwapChain = needRecreateSwapChain
            , _frameIndex = nextFrameIndex
            , _renderTargetData = renderTargetData
            , _renderPassDataList = renderPassDataList
            , _rendererData = rendererData }

    terminateApplication finalApplicationData
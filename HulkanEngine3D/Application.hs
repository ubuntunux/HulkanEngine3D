{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}

module HulkanEngine3D.Application
    ( KeyboardInputData (..)
    , KeyboardInputInterface (..)
    , ApplicationData (..)
    , runApplication
    ) where


import Data.Hashable
import Control.Monad
import Data.IORef
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.DList as DList
import System.Directory
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Control.Lens
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

instance Hashable GLFW.Key

type KeyMap = HashTable.BasicHashTable GLFW.Key Bool

data KeyboardInputData = KeyboardInputData
    { _keyboardDown :: Bool
    , _keyboardPressed :: Bool
    , _keyboardUp :: Bool
    , _keyPressedMap :: KeyMap
    , _keyReleasedMap :: KeyMap
    } deriving (Show)

makeLenses ''KeyboardInputData

class KeyboardInputInterface a where
    getKeyPressed :: a -> GLFW.Key -> IO Bool
    getKeyReleased :: a -> GLFW.Key -> IO Bool

instance KeyboardInputInterface KeyboardInputData where
    getKeyPressed keyboardInputData key = fromMaybe False <$> HashTable.lookup (keyboardInputData^.keyPressedMap) key
    getKeyReleased keyboardInputData key = fromMaybe False <$> HashTable.lookup (keyboardInputData^.keyReleasedMap) key



data ApplicationData = ApplicationData
    { _window :: GLFW.Window
    , _windowSizeChanged :: IORef Bool
    , _needRecreateSwapChain :: Bool
    , _frameIndex  :: Int
    , _imageIndexPtr :: Ptr Word32
    , _accFrameTime :: Double
    , _accFrameCount :: Int
    , _averageFrameTime :: Double
    , _averageFPS :: Double
    , _currentTime :: Double
    , _elapsedTime :: Double
    , _keyboardInputDataRef :: IORef KeyboardInputData
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

makeLenses ''ApplicationData


createGLFWWindow::Int -> Int -> String -> IORef Bool -> IORef KeyboardInputData -> IO (Maybe GLFW.Window)
createGLFWWindow width height title windowSizeChanged keyboardInputDataRef = do
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
    GLFW.setKeyCallback window $ Just (keyCallBack keyboardInputDataRef)
    GLFW.setCharCallback window $ Just charCallBack
    return maybeWindow

destroyGLFWWindow :: GLFW.Window -> IO ()
destroyGLFWWindow window = do
    GLFW.destroyWindow window >> logInfo "Closed GLFW window."
    GLFW.terminate >> logInfo "Terminated GLFW."


updateEvent :: ApplicationData -> IO ApplicationData
updateEvent applicationData = do
    keyboardInputData <- readIORef (_keyboardInputDataRef applicationData)
    writeIORef (_keyboardInputDataRef applicationData) $ keyboardInputData
        { _keyboardDown = False
        , _keyboardUp = False }
    return applicationData


keyCallBack :: IORef KeyboardInputData -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallBack keyboardInputDataRef window key scanCode keyState modifierKeys = do
    keyboardInputData <- readIORef keyboardInputDataRef
    let keyboardPressed = (GLFW.KeyState'Pressed == keyState)
        keyboardDown = (GLFW.KeyState'Pressed == keyState)
        keyboardUp = (GLFW.KeyState'Released == keyState)
        keyPressedMap = (_keyPressedMap keyboardInputData)
        keyReleasedMap = (_keyReleasedMap keyboardInputData)
    HashTable.insert keyPressedMap key keyboardPressed
    HashTable.insert keyReleasedMap key (not keyboardPressed)
    writeIORef keyboardInputDataRef $ keyboardInputData
        { _keyboardPressed = keyboardPressed
        , _keyboardDown = keyboardDown
        , _keyboardUp = keyboardUp
        , _keyPressedMap = keyPressedMap
        , _keyReleasedMap = keyReleasedMap }

charCallBack :: GLFW.Window -> Char -> IO ()
charCallBack windows key = do
    -- logInfo $ show key
    return ()

initializeApplication :: IO ApplicationData
initializeApplication = do
    windowSizeChanged <- newIORef False
    keyPressed <- HashTable.new
    keyReleased <- HashTable.new
    let keyboardInputData = KeyboardInputData
            { _keyboardDown = False
            , _keyboardPressed = False
            , _keyboardUp = False
            , _keyPressedMap = keyPressed
            , _keyReleasedMap = keyReleased
            }
    keyboardInputDataRef <- newIORef keyboardInputData
    maybeWindow <- createGLFWWindow 1024 768 "Vulkan Application" windowSizeChanged keyboardInputDataRef
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

    return ApplicationData
            { _window = window
            , _needRecreateSwapChain = False
            , _windowSizeChanged = windowSizeChanged
            , _frameIndex = 0
            , _imageIndexPtr = imageIndexPtr
            , _accFrameTime = 0.0
            , _accFrameCount = 0
            , _averageFrameTime = 0.0
            , _averageFPS = 0.0
            , _currentTime = currentTime
            , _elapsedTime = 0.0
            , _keyboardInputDataRef = keyboardInputDataRef
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
    inpuData <- readIORef (_keyboardInputDataRef applicationData)
    escReleased <- getKeyReleased inpuData GLFW.Key'Escape
    exit <- GLFW.windowShouldClose (view window applicationData)
    if not exit && not escReleased then do
        applicationData <- updateEvent applicationData
        GLFW.pollEvents
        applicationData <- loopAction applicationData
        updateLoop applicationData loopAction
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
            accFrameTime = (_accFrameTime applicationData) + deltaTime
            accFrameCount = (_accFrameCount applicationData) + 1
        (accFrameTime, accFrameCount, averageFrameTime, averageFPS) <- if (1.0 < accFrameTime)
            then do
                let averageFrameTime = accFrameTime / (fromIntegral accFrameCount) * 1000.0
                    averageFPS = 1000.0 / averageFrameTime
                logInfo $ show averageFPS ++ "fps / " ++ show averageFrameTime ++ "ms"
                return (0.0, 0, averageFrameTime, averageFPS)
            else
                return (accFrameTime, accFrameCount, (_averageFrameTime applicationData), (_averageFPS applicationData))

        let frameIndex = (_frameIndex applicationData)

        applicationData <-
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

                return $ applicationData
                    { _renderTargetData = renderTargetData
                    , _renderPassDataList = renderPassDataList
                    , _rendererData = rendererData }
            else
                return applicationData

        result <- drawFrame (_rendererData applicationData) frameIndex (_imageIndexPtr applicationData) (_transformObjectMemories applicationData)

        -- waiting
        deviceWaitIdle (_rendererData applicationData)

        sizeChanged <- readIORef (_windowSizeChanged applicationData)
        let needRecreateSwapChain = (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged)
        when needRecreateSwapChain $ atomicWriteIORef (_windowSizeChanged applicationData) False

        let nextFrameIndex = mod (frameIndex + 1) Constants.maxFrameCount

        return applicationData
            { _currentTime = currentTime
            , _elapsedTime = elapsedTime
            , _accFrameTime = accFrameTime
            , _accFrameCount = accFrameCount
            , _averageFrameTime = averageFrameTime
            , _averageFPS = averageFPS
            , _needRecreateSwapChain = needRecreateSwapChain
            , _frameIndex = nextFrameIndex }

    terminateApplication finalApplicationData
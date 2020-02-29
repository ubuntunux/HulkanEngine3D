{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}

module HulkanEngine3D.Application
    ( KeyboardInputData (..)
    , KeyboardInputInterface (..)
    , MouseInputData (..)
    , ApplicationData (..)
    , runApplication
    ) where


import Data.Hashable
import Control.Monad
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.DList as DList
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Control.Lens
import qualified Data.HashTable.IO as HashTable
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain


import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Application.SceneManager
import HulkanEngine3D.Render.Camera
import HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Resource.ObjLoader
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan
import HulkanEngine3D.Vulkan.Mesh
import HulkanEngine3D.Vulkan.Device
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.TransformationObject

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

data MouseInputData = MouseInputData
    { _mousePosX :: Int
    , _mousePosY :: Int
    } deriving (Show)

makeLenses ''MouseInputData

data ApplicationData = ApplicationData
    { _window :: GLFW.Window
    , _windowSizeChangedRef :: IORef Bool
    , _windowSizeRef :: IORef (Int, Int)
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
    , _mouseInputDataRef :: IORef MouseInputData
    , _renderTargetData :: RenderTargetData
    , _sceneManagerData :: SceneManagerData
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


class KeyboardInputInterface a where
    getKeyPressed :: a -> GLFW.Key -> IO Bool
    getKeyReleased :: a -> GLFW.Key -> IO Bool

instance KeyboardInputInterface KeyboardInputData where
    getKeyPressed keyboardInputData key = fromMaybe False <$> HashTable.lookup (keyboardInputData^.keyPressedMap) key
    getKeyReleased keyboardInputData key = fromMaybe False <$> HashTable.lookup (keyboardInputData^.keyReleasedMap) key


mouseButtonCallback :: IORef MouseInputData -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback mouseInputDataRef window mouseButton mouseButtonState modifierKeys = do
    mouseInputData <- readIORef mouseInputDataRef
    writeIORef mouseInputDataRef mouseInputData
--    logInfo $ show mouseButton
--    logInfo $ show mouseButtonState
--    logInfo $ show modifierKeys

cursorPosCallback :: IORef MouseInputData -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback mouseInputDataRef windows posX posY = do
    mouseInputData <- readIORef mouseInputDataRef
    writeIORef mouseInputDataRef $ mouseInputData
        { _mousePosX = round posX
        , _mousePosY = round posY }
--    logInfo $ show (posX, posY)

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

windowSizeCallback :: IORef Bool -> IORef (Int, Int) -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback windowSizeChangedRef windowSizeRef window sizeX sizeY = do
    atomicWriteIORef windowSizeChangedRef True
    atomicWriteIORef windowSizeRef (sizeX, sizeY)

createGLFWWindow :: String -> IORef (Int, Int) -> IORef Bool -> IORef KeyboardInputData -> IORef MouseInputData -> IO GLFW.Window
createGLFWWindow title windowSizeRef windowSizeChangedRef keyboardInputDataRef mouseInputDataRef = do
    GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")
    logInfo "Initialized GLFW."
    Just version <- GLFW.getVersionString
    logInfo $ ("GLFW Version: " ++) version
    (width, height) <- readIORef windowSizeRef
    GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
    GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    GLFW.windowHint $ WindowHint'Resizable True
    Just window <- GLFW.createWindow width height title Nothing Nothing
    GLFW.setWindowSizeCallback window $ Just (windowSizeCallback windowSizeChangedRef windowSizeRef)
    GLFW.setKeyCallback window $ Just (keyCallBack keyboardInputDataRef)
    GLFW.setCharCallback window $ Just charCallBack
    GLFW.setMouseButtonCallback window $ Just (mouseButtonCallback mouseInputDataRef)
    GLFW.setCursorPosCallback window $ Just (cursorPosCallback mouseInputDataRef)
    return window

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

initializeApplication :: IO ApplicationData
initializeApplication = do
    keyPressed <- HashTable.new
    keyReleased <- HashTable.new
    keyboardInputDataRef <- newIORef $ KeyboardInputData
        { _keyboardDown = False
        , _keyboardPressed = False
        , _keyboardUp = False
        , _keyPressedMap = keyPressed
        , _keyReleasedMap = keyReleased }
    mouseInputDataRef <- newIORef $ MouseInputData
        { _mousePosX = 0
        , _mousePosY = 0 }
    let (width, height) = (1024, 768)
    windowSizeChangedRef <- newIORef False
    windowSizeRef <- newIORef (1024, 768)
    window <- createGLFWWindow "Vulkan Application" windowSizeRef windowSizeChangedRef keyboardInputDataRef mouseInputDataRef
    logInfo "                             "
    logInfo "<< Initialized GLFW window >>"
    requireExtensions <- GLFW.getRequiredInstanceExtensions
    instanceExtensionNames <- getInstanceExtensionSupport
    checkExtensionResult <- checkExtensionSupport instanceExtensionNames requireExtensions
    unless checkExtensionResult (throwVKMsg "Failed to initialize GLFW window.")

    let progName = "Hulkan App"
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

    -- SceneManagerDatas
    (width, height) <- readIORef windowSizeRef
    let aspect = if 0 /= height then (((fromIntegral width)::Float) / ((fromIntegral height)::Float)) else 1.0
        cameraData = getDefaultCameraData Constants.near Constants.far Constants.fov aspect
        sceneManagerData = getDefaultSceneManagerData cameraData

    -- init system variables
    imageIndexPtr <- new (0 :: Word32)
    currentTime <- getSystemTime

    return ApplicationData
            { _window = window
            , _windowSizeChangedRef = windowSizeChangedRef
            , _windowSizeRef = windowSizeRef
            , _needRecreateSwapChain = False
            , _frameIndex = 0
            , _imageIndexPtr = imageIndexPtr
            , _accFrameTime = 0.0
            , _accFrameCount = 0
            , _averageFrameTime = 0.0
            , _averageFPS = 0.0
            , _currentTime = currentTime
            , _elapsedTime = 0.0
            , _keyboardInputDataRef = keyboardInputDataRef
            , _mouseInputDataRef = mouseInputDataRef
            , _renderTargetData = renderTargetData
            , _sceneManagerData = sceneManagerData
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
        mouseInputData <- readIORef (_mouseInputDataRef applicationData)
        mousePos <- pure (mouseInputData^.mousePosX, mouseInputData^.mousePosY)
        result <- drawFrame (_rendererData applicationData) frameIndex (_imageIndexPtr applicationData) (_transformObjectMemories applicationData) mousePos

        -- waiting
        deviceWaitIdle (_rendererData applicationData)

        sizeChanged <- readIORef (applicationData^.windowSizeChangedRef)
        let needRecreateSwapChain = (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged)
        when needRecreateSwapChain $ atomicWriteIORef (applicationData^.windowSizeChangedRef) False

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
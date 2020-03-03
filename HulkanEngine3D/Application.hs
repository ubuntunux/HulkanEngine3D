{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Lens
import qualified Data.HashTable.IO as HashTable
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Numeric.DataFrame

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Application.SceneManager
import HulkanEngine3D.Render.Camera
import HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Render.TransformObject
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
    , _modifierKeys :: GLFW.ModifierKeys
    } deriving (Show)

makeLenses ''KeyboardInputData

data MouseInputData = MouseInputData
    { _mousePosX :: Int
    , _mousePosY :: Int
    } deriving (Show)

makeLenses ''MouseInputData

data TimeData = TimeData
    { _accFrameTime :: Double
    , _accFrameCount :: Int
    , _averageFrameTime :: Double
    , _averageFPS :: Double
    , _currentTime :: Double
    , _elapsedTime :: Double
    , _deltaTime :: Double
    } deriving (Show)

makeLenses ''TimeData

data ApplicationData = ApplicationData
    { _window :: GLFW.Window
    , _windowSizeChangedRef :: IORef Bool
    , _windowSizeRef :: IORef (Int, Int)
    , _needRecreateSwapChainRef :: IORef Bool
    , _frameIndexRef  :: IORef Int
    , _timeDataRef :: IORef TimeData
    , _keyboardInputDataRef :: IORef KeyboardInputData
    , _mouseInputDataRef :: IORef MouseInputData
    , _renderTargetDataRef :: IORef RenderTargetData
    , _sceneManagerData :: SceneManagerData
    , _rendererDataRef :: IORef RendererData
    , _renderPassDataListRef :: IORef (DList.DList RenderPassData)
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
    let keyboardPressed = (GLFW.KeyState'Pressed == keyState || GLFW.KeyState'Repeating == keyState)
        keyboardReleased = (GLFW.KeyState'Released == keyState)
        keyPressedMap = (_keyPressedMap keyboardInputData)
        keyReleasedMap = (_keyReleasedMap keyboardInputData)
    HashTable.insert keyPressedMap key keyboardPressed
    HashTable.insert keyReleasedMap key (not keyboardPressed)
    writeIORef keyboardInputDataRef $ keyboardInputData
        { _keyboardPressed = keyboardPressed
        , _keyboardDown = keyboardPressed
        , _keyboardUp = keyboardReleased
        , _keyPressedMap = keyPressedMap
        , _keyReleasedMap = keyReleasedMap
        , _modifierKeys = modifierKeys }

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

updateEvent :: ApplicationData -> IO ()
updateEvent applicationData = do
    keyboardInputData <- readIORef (_keyboardInputDataRef applicationData)
    pressed_key_A <- getKeyPressed keyboardInputData GLFW.Key'A
    pressed_key_D <- getKeyPressed keyboardInputData GLFW.Key'D
    pressed_key_W <- getKeyPressed keyboardInputData GLFW.Key'W
    pressed_key_S <- getKeyPressed keyboardInputData GLFW.Key'S
    timeData <- readIORef (_timeDataRef applicationData)

    let deltaTime = (realToFrac._deltaTime $ timeData)::Float
        modifierKeysShift = (GLFW.modifierKeysShift._modifierKeys $ keyboardInputData)
        move_speed = deltaTime * if modifierKeysShift then 2.0 else 1.0
        move_speed_side = (* move_speed) $ case (pressed_key_A, pressed_key_D) of
            (True, False) -> Constants.cameraMoveSpeed
            (False, True) -> -Constants.cameraMoveSpeed
            otherwise -> 0.0
        move_speed_forward = (* move_speed) $ case (pressed_key_W, pressed_key_S) of
            (True, False) -> Constants.cameraMoveSpeed
            (False, True) -> -Constants.cameraMoveSpeed
            otherwise -> 0.0
        cameraPositionRef = (_position._transformObject._camera._sceneManagerData $ applicationData)
    cameraPosition <- readIORef cameraPositionRef
    writeIORef cameraPositionRef $ ewmap (\(Vec3 x y z) -> vec3 (x + move_speed_side) y (z + move_speed_forward)) cameraPosition

initializeApplication :: IO ApplicationData
initializeApplication = do
    keyPressed <- HashTable.new
    keyReleased <- HashTable.new
    keyboardInputDataRef <- newIORef $ KeyboardInputData
        { _keyboardDown = False
        , _keyboardPressed = False
        , _keyboardUp = False
        , _keyPressedMap = keyPressed
        , _keyReleasedMap = keyReleased
        , _modifierKeys = GLFW.ModifierKeys
            { GLFW.modifierKeysShift = False
            , GLFW.modifierKeysControl = False
            , GLFW.modifierKeysAlt = False
            , GLFW.modifierKeysSuper = False
            }
        }
    mouseInputDataRef <- newIORef $ MouseInputData
        { _mousePosX = 0
        , _mousePosY = 0 }
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
    rendererDataRef <- newIORef rendererData

    -- create render targets
    renderTargetData <- createRenderTargets rendererData
    renderTargetDataRef <- newIORef renderTargetData

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

    -- SceneManagerDatas
    (width, height) <- readIORef windowSizeRef
    let aspect = if 0 /= height then (fromIntegral width / fromIntegral height)::Float else 1.0
    cameraData <- getDefaultCameraData Constants.near Constants.far Constants.fov aspect
    let sceneManagerData = getDefaultSceneManagerData cameraData

    -- init system variables
    needRecreateSwapChainRef <- newIORef False
    frameIndexRef <- newIORef (0::Int)
    currentTime <- getSystemTime
    timeDataRef <- newIORef TimeData
        { _accFrameTime = 0.0
        , _accFrameCount = 0
        , _averageFrameTime = 0.0
        , _averageFPS = 0.0
        , _currentTime = currentTime
        , _elapsedTime = 0.0
        , _deltaTime = 0.0
        }

    return ApplicationData
            { _window = window
            , _windowSizeChangedRef = windowSizeChangedRef
            , _windowSizeRef = windowSizeRef
            , _needRecreateSwapChainRef = needRecreateSwapChainRef
            , _frameIndexRef = frameIndexRef
            , _timeDataRef = timeDataRef
            , _keyboardInputDataRef = keyboardInputDataRef
            , _mouseInputDataRef = mouseInputDataRef
            , _renderTargetDataRef = renderTargetDataRef
            , _sceneManagerData = sceneManagerData
            , _rendererDataRef = rendererDataRef
            , _renderPassDataListRef = renderPassDataListRef
            , _transformObjectBuffers = transformObjectBuffers
            , _transformObjectMemories = transformObjectMemories
            , _descriptorPool = descriptorPool
            , _descriptorSetData = descriptorSetData
            , _textureData = textureData
            , _geometryBuffer = geometryBuffer
            }

updateLoop :: ApplicationData -> (ApplicationData -> IO ()) -> IO ()
updateLoop applicationData loopAction = do
    keyboardInputData <- readIORef (_keyboardInputDataRef applicationData)
    escReleased <- getKeyReleased keyboardInputData GLFW.Key'Escape
    exit <- GLFW.windowShouldClose (_window applicationData)
    when (not exit && not escReleased) $ do
        -- reset input flags
        writeIORef (_keyboardInputDataRef applicationData) keyboardInputData
            { _keyboardDown = False
            , _keyboardUp = False }

        GLFW.pollEvents

        updateEvent applicationData
        loopAction applicationData
        updateLoop applicationData loopAction

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

    renderTargetData <- readIORef (_renderTargetDataRef applicationData)
    destroyTexture rendererData (_sceneColorTexture renderTargetData)
    destroyTexture rendererData (_sceneDepthTexture renderTargetData)

    destroyTexture rendererData (_textureData applicationData)

    destroyGeometryBuffer rendererData (_geometryBuffer applicationData)

    renderPassDataList <- readIORef (_renderPassDataListRef applicationData)
    forM_ renderPassDataList $ \renderPassData -> do
        destroyRenderPassData (_device rendererData) renderPassData

    destroyRenderer rendererData

    destroyGLFWWindow (_window applicationData)


updateTimeData :: IORef TimeData -> IO ()
updateTimeData timeDataRef = do
    currentTime <- getSystemTime
    timeData <- readIORef timeDataRef
    let previousTime = _currentTime timeData
        deltaTime = currentTime - previousTime
        elapsedTime = (_elapsedTime timeData) + deltaTime
        accFrameTime = (_accFrameTime timeData) + deltaTime
        accFrameCount = (_accFrameCount timeData) + 1
    (accFrameTime, accFrameCount, averageFrameTime, averageFPS) <- if (1.0 < accFrameTime)
        then do
            let averageFrameTime = accFrameTime / (fromIntegral accFrameCount) * 1000.0
                averageFPS = 1000.0 / averageFrameTime
            logInfo $ show averageFPS ++ "fps / " ++ show averageFrameTime ++ "ms"
            return (0.0, 0, averageFrameTime, averageFPS)
        else
            return (accFrameTime, accFrameCount, (_averageFrameTime timeData), (_averageFPS timeData))
    writeIORef timeDataRef TimeData
        { _deltaTime = deltaTime
        , _currentTime = currentTime
        , _elapsedTime = elapsedTime
        , _accFrameTime = accFrameTime
        , _accFrameCount = accFrameCount
        , _averageFrameTime = averageFrameTime
        , _averageFPS = averageFPS
        }

runApplication :: IO ()
runApplication = do
    applicationData <- initializeApplication

    -- Main Loop
    updateLoop applicationData $ \applicationData -> do
        updateTimeData $ _timeDataRef applicationData

        frameIndex <- readIORef (_frameIndexRef applicationData)
        needRecreateSwapChain <- readIORef (_needRecreateSwapChainRef applicationData)
        when needRecreateSwapChain $ do
            logInfo "                        "
            logInfo "<< Recreate SwapChain >>"

            -- cleanUp swapChain
            rendererData <- readIORef (_rendererDataRef applicationData)

            -- waiting
            deviceWaitIdle rendererData

            renderPassDataList <- readIORef (_renderPassDataListRef applicationData)
            forM_ renderPassDataList $ \renderPassData -> do
                destroyRenderPass rendererData renderPassData

            renderTargetData <- readIORef (_renderTargetDataRef applicationData)
            destroyTexture rendererData (_sceneColorTexture renderTargetData)
            destroyTexture rendererData (_sceneDepthTexture renderTargetData)

            -- recreate swapChain
            rendererData <- recreateSwapChain rendererData (_window applicationData)
            writeIORef (_rendererDataRef applicationData) rendererData

            -- recreate resources
            renderTargetData <- createRenderTargets rendererData
            writeIORef (_renderTargetDataRef applicationData) renderTargetData

            renderPassDataCreateInfo <- getDefaultRenderPassDataCreateInfo
                rendererData
                [(_imageView (_sceneColorTexture renderTargetData)), (_imageView (_sceneDepthTexture renderTargetData))]
                [getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0]
            renderPassData <- createRenderPassData (getDevice rendererData) renderPassDataCreateInfo
            writeIORef (_renderPassDataListRef applicationData) (DList.fromList [renderPassData])

            -- record render commands
            let vertexBuffer = _vertexBuffer (_geometryBuffer applicationData)
                vertexIndexCount = _vertexIndexCount (_geometryBuffer applicationData)
                indexBuffer = _indexBuffer (_geometryBuffer applicationData)
                descriptorSetData = (_descriptorSetData applicationData)
            recordCommandBuffer rendererData renderPassData vertexBuffer (vertexIndexCount, indexBuffer) (_descriptorSets descriptorSetData)

        rendererData <- readIORef (_rendererDataRef applicationData)
        cameraPosition <- readIORef (_position._transformObject._camera._sceneManagerData $ applicationData)
        result <- drawFrame rendererData frameIndex (_transformObjectMemories applicationData) cameraPosition

        -- waiting
        deviceWaitIdle rendererData

        sizeChanged <- readIORef (applicationData^.windowSizeChangedRef)
        let needRecreateSwapChain = (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged)
        when needRecreateSwapChain $ atomicWriteIORef (applicationData^.windowSizeChangedRef) False

        writeIORef (_frameIndexRef applicationData) $ mod (frameIndex + 1) Constants.maxFrameCount
        writeIORef (_needRecreateSwapChainRef applicationData) needRecreateSwapChain

    terminateApplication applicationData
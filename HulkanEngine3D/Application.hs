{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module HulkanEngine3D.Application
    ( ApplicationData (..)
    , runApplication
    ) where

import Control.Monad
import Data.IORef
import qualified Data.HashTable.IO as HashTable
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame
import Numeric.Dimensions

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Application.Input
import HulkanEngine3D.Application.SceneManager
import HulkanEngine3D.Render.Camera
import HulkanEngine3D.Render.Mesh
import qualified HulkanEngine3D.Render.Renderer as Renderer
import HulkanEngine3D.Render.TransformObject
import HulkanEngine3D.Resource.Resource
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Device
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.SceneConstants


data TimeData = TimeData
    { _accFrameTime :: Double
    , _accFrameCount :: Int
    , _averageFrameTime :: Double
    , _averageFPS :: Double
    , _currentTime :: Double
    , _elapsedTime :: Double
    , _deltaTime :: Double
    } deriving (Show)

data ApplicationData = ApplicationData
    { _window :: GLFW.Window
    , _windowSizeChangedRef :: IORef Bool
    , _windowSizeRef :: IORef (Int, Int)
    , _timeDataRef :: IORef TimeData
    , _keyboardInputDataRef :: IORef KeyboardInputData
    , _mouseMoveDataRef :: IORef MouseMoveData
    , _mouseInputDataRef :: IORef MouseInputData
    , _sceneManagerData :: SceneManagerData
    , _rendererData :: Renderer.RendererData
    , _resourceData :: ResourceData
    , _sceneConstantsBuffers :: [VkBuffer]
    , _sceneConstantsMemories :: [VkDeviceMemory]
    , _descriptorSetData :: DescriptorSetData
    } deriving (Show)


class ApplicationInterface a where
    getDeltaTime :: a -> IO Float

instance ApplicationInterface ApplicationData where
    getDeltaTime applicationData = do
        timeData <- readIORef (_timeDataRef applicationData)
        return . realToFrac . _deltaTime $ timeData


mouseButtonCallback :: IORef MouseInputData -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback mouseInputDataRef window mouseButton mouseButtonState modifierKeys = do
    mouseInputData <- readIORef mouseInputDataRef
    let (down, up) = if GLFW.MouseButtonState'Pressed == mouseButtonState
        then (True, False)
        else (False, True)
    writeIORef mouseInputDataRef $ getMouseInputData mouseInputData mouseButton (down, up)
    where
        getMouseInputData :: MouseInputData -> GLFW.MouseButton -> (Bool, Bool) -> MouseInputData
        getMouseInputData mouseInputData GLFW.MouseButton'1 (down, up) = mouseInputData { _btn_l_down = down, _btn_l_up = up }
        getMouseInputData mouseInputData GLFW.MouseButton'2 (down, up) = mouseInputData { _btn_r_down = down, _btn_r_up = up }
        getMouseInputData mouseInputData GLFW.MouseButton'3 (down, up) = mouseInputData { _btn_m_down = down, _btn_m_up = up }
        getMouseInputData mouseInputData _ (down, up) = mouseInputData

cursorPosCallback :: IORef MouseMoveData -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback mouseMoveDataRef windows posX posY = do
    mouseMoveData <- readIORef mouseMoveDataRef
    let newPos = vec2 (round posX) (round posY)
        posDelta = newPos - _mousePosPrev mouseMoveData
    writeIORef mouseMoveDataRef $ mouseMoveData
        { _mousePos = newPos
        , _mousePosDelta = posDelta
        }

keyCallBack :: IORef KeyboardInputData -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallBack keyboardInputDataRef window key scanCode keyState modifierKeys = do
    keyboardInputData <- readIORef keyboardInputDataRef
    let keyboardPressed = GLFW.KeyState'Pressed == keyState || GLFW.KeyState'Repeating == keyState
        keyboardReleased = GLFW.KeyState'Released == keyState
        keyPressedMap = _keyPressedMap keyboardInputData
        keyReleasedMap = _keyReleasedMap keyboardInputData
    HashTable.insert keyPressedMap key keyboardPressed
    HashTable.insert keyReleasedMap key (not keyboardPressed)
    writeIORef keyboardInputDataRef $ keyboardInputData
        { _keyboardPressed = keyboardPressed
        , _keyboardDown = keyboardPressed
        , _keyboardUp = keyboardReleased
        , _modifierKeys = modifierKeys }

charCallBack :: GLFW.Window -> Char -> IO ()
charCallBack windows key = do
    -- logInfo $ show key
    return ()

windowSizeCallback :: IORef Bool -> IORef (Int, Int) -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback windowSizeChangedRef windowSizeRef window sizeX sizeY = do
    atomicWriteIORef windowSizeChangedRef True
    atomicWriteIORef windowSizeRef (sizeX, sizeY)


createGLFWWindow :: String
                 -> IORef (Int, Int)
                 -> IORef Bool
                 -> IORef KeyboardInputData
                 -> IORef MouseInputData
                 -> IORef MouseMoveData
                 -> IO GLFW.Window
createGLFWWindow title windowSizeRef windowSizeChangedRef keyboardInputDataRef mouseInputDataRef mouseMoveDataRef = do
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
    GLFW.setCursorPosCallback window $ Just (cursorPosCallback mouseMoveDataRef)
    return window

destroyGLFWWindow :: GLFW.Window -> IO ()
destroyGLFWWindow window = do
    GLFW.destroyWindow window >> logInfo "Closed GLFW window."
    GLFW.terminate >> logInfo "Terminated GLFW."

updateEvent :: ApplicationData -> IO ()
updateEvent applicationData = do
    deltaTime <- getDeltaTime applicationData
    keyboardInputData <- readIORef (_keyboardInputDataRef applicationData)
    mouseInputData <- readIORef (_mouseInputDataRef applicationData)
    mouseMoveData <- readIORef (_mouseMoveDataRef applicationData)
    pressed_key_A <- getKeyPressed keyboardInputData GLFW.Key'A
    pressed_key_D <- getKeyPressed keyboardInputData GLFW.Key'D
    pressed_key_W <- getKeyPressed keyboardInputData GLFW.Key'W
    pressed_key_S <- getKeyPressed keyboardInputData GLFW.Key'S
    pressed_key_Q <- getKeyPressed keyboardInputData GLFW.Key'Q
    pressed_key_E <- getKeyPressed keyboardInputData GLFW.Key'E
    mainCamera <- readIORef . _mainCamera . _sceneManagerData $ applicationData
    let mousePosDelta = _mousePosDelta mouseMoveData
        mousePosDeltaX = fromIntegral . unScalar $ (mousePosDelta ! (Idx 0:*U)) :: Float
        mousePosDeltaY = fromIntegral . unScalar $ (mousePosDelta ! (Idx 1:*U)) :: Float
        (_, btn_middle, btn_right, _, _) = (_btn_l_down mouseInputData, _btn_m_down mouseInputData, _btn_r_down mouseInputData, _wheel_up mouseInputData, _wheel_down mouseInputData)
        modifierKeysShift = (GLFW.modifierKeysShift._modifierKeys $ keyboardInputData)
        moveSpeed = Constants.cameraMoveSpeed * deltaTime * if modifierKeysShift then 2.0 else 1.0
        panSpeed = Constants.cameraPanSpeed * (if modifierKeysShift then 2.0 else 1.0)
        rotationSpeed = Constants.cameraRotationSpeed
        cameraTransformObject = _transformObject mainCamera

    if btn_middle then do
        moveLeft cameraTransformObject (-panSpeed * mousePosDeltaX)
        moveUp cameraTransformObject (panSpeed * mousePosDeltaY)
    else when btn_right $ do
        rotationPitch cameraTransformObject (-rotationSpeed * mousePosDeltaY)
        rotationYaw cameraTransformObject (-rotationSpeed * mousePosDeltaX)

    if pressed_key_W then
        moveFront cameraTransformObject (-moveSpeed)
    else when pressed_key_S $
        moveFront cameraTransformObject moveSpeed

    if pressed_key_A then
        moveLeft cameraTransformObject (-moveSpeed)
    else when pressed_key_D $
        moveLeft cameraTransformObject moveSpeed

    if pressed_key_Q then
        moveUp cameraTransformObject (-moveSpeed)
    else when pressed_key_E $
        moveUp cameraTransformObject moveSpeed


initializeApplication :: IO ApplicationData
initializeApplication = do
    let (width, height) = (1024 :: Int, 786)
        mousePos = vec2 (div width 2) (div height 2)
    keyboardInputData <- newKeyboardInputData
    keyboardInputDataRef <- newIORef keyboardInputData
    mouseMoveDataRef <- newIORef $ newMouseMoveData mousePos
    mouseInputDataRef <- newIORef newMouseInputData
    windowSizeChangedRef <- newIORef False
    windowSizeRef <- newIORef (width, height)
    window <- createGLFWWindow "Vulkan Application" windowSizeRef windowSizeChangedRef keyboardInputDataRef mouseInputDataRef mouseMoveDataRef
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

    resourceData <- createNewResourceData
    rendererData <- Renderer.createRenderer
        window
        progName
        engineName
        enableValidationLayer
        isConcurrentMode
        requireExtensions
        msaaSampleCount
        resourceData
    Renderer.initializeRenderer rendererData

    initializeResourceData resourceData rendererData

    (Just textureData) <- getTextureData resourceData "texture"

    (sceneConstantsMemories, sceneConstantsBuffers) <- unzip <$> createSceneConstantsBuffers
        (Renderer.getPhysicalDevice rendererData)
        (Renderer.getDevice rendererData)
        Constants.swapChainImageCount
        
    defaultRenderPassData <- getDefaultRenderPassData resourceData
    
    descriptorSetData <- createDescriptorSetData
        (Renderer.getDevice rendererData)
        (Renderer.getDescriptorPool rendererData)
        Constants.swapChainImageCount
        (getDescriptorSetLayout defaultRenderPassData)

    let descriptorBufferInfos = fmap (\buffer -> createDescriptorBufferInfo buffer sceneConstantsBufferSize) sceneConstantsBuffers

    forM_ (zip descriptorBufferInfos (_descriptorSets descriptorSetData)) $ \(descriptorBufferInfo, descriptorSet) ->
        prepareDescriptorSet (Renderer.getDevice rendererData) descriptorBufferInfo (getTextureImageInfo textureData) descriptorSet

    -- SceneManagerDatas
    sceneManagerData <- newSceneManagerData

    let aspect = if 0 /= height then (fromIntegral width / fromIntegral height)::Float else 1.0
        cameraCreateData = getDefaultCameraCreateData { aspect = aspect, position = vec3 0 0 10 }
    initializeSceneManagerData sceneManagerData cameraCreateData

    -- init system variables
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
            , _timeDataRef = timeDataRef
            , _keyboardInputDataRef = keyboardInputDataRef
            , _mouseMoveDataRef = mouseMoveDataRef
            , _mouseInputDataRef = mouseInputDataRef
            , _sceneManagerData = sceneManagerData
            , _rendererData = rendererData
            , _resourceData = resourceData
            , _sceneConstantsBuffers = sceneConstantsBuffers
            , _sceneConstantsMemories = sceneConstantsMemories
            , _descriptorSetData = descriptorSetData
            }

updateLoop :: ApplicationData -> (ApplicationData -> IO ()) -> IO ()
updateLoop applicationData loopAction = do
    moveMoveData <- readIORef (_mouseMoveDataRef applicationData)
    keyboardInputData <- readIORef (_keyboardInputDataRef applicationData)
    escReleased <- getKeyReleased keyboardInputData GLFW.Key'Escape
    exit <- GLFW.windowShouldClose (_window applicationData)
    when (not exit && not escReleased) $ do
        -- reset input flags
        writeIORef (_keyboardInputDataRef applicationData) keyboardInputData
            { _keyboardDown = False
            , _keyboardUp = False
            }
        writeIORef (_mouseMoveDataRef applicationData) moveMoveData
            { _mousePosDelta = vec2 0 0
            , _mousePosPrev = _mousePos moveMoveData
            }

        GLFW.pollEvents

        updateEvent applicationData
        loopAction applicationData
        updateLoop applicationData loopAction

terminateApplication :: ApplicationData -> IO ()
terminateApplication applicationData = do
    logInfo "<< Terminate >>"

    let rendererData = (_rendererData applicationData)

    -- waiting
    Renderer.deviceWaitIdle rendererData

    destroySceneConstantsBuffers
        (Renderer.getDevice rendererData)
        (_sceneConstantsBuffers applicationData)
        (_sceneConstantsMemories applicationData)

    destroyResourceData (_resourceData applicationData) rendererData
    Renderer.destroyRenderer rendererData
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
    (accFrameTime, accFrameCount, averageFrameTime, averageFPS) <-
        if (1.0 < accFrameTime) then do
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

        let rendererData = _rendererData applicationData

        -- resize window
        needRecreateSwapChain <- readIORef (Renderer._needRecreateSwapChainRef rendererData)
        windowSizeChanged <- readIORef (_windowSizeChangedRef applicationData)
        when (windowSizeChanged || needRecreateSwapChain) $ do
            Renderer.resizeWindow (_window applicationData) rendererData
            writeIORef (_windowSizeChangedRef applicationData) False
            writeIORef (Renderer._needRecreateSwapChainRef rendererData) False
            (width, height) <- readIORef (_windowSizeRef applicationData)
            let aspect = if 0 /= height then (fromIntegral width / fromIntegral height)::Float else 1.0
            mainCamera <- getMainCamera (_sceneManagerData applicationData)
            setAspect mainCamera aspect

        -- update renderer data
        updateSceneManagerData (_sceneManagerData applicationData)
        mainCamera <- getMainCamera (_sceneManagerData applicationData)
        viewMatrix <- readIORef (_viewMatrix mainCamera)
        projectionMatrix <- readIORef (_projectionMatrix mainCamera)
        (Just meshData) <- getMeshData (_resourceData applicationData) "suzan"
        geometryBufferData <- (getGeometryData meshData 0)

        Renderer.updateRendererData
            rendererData
            viewMatrix
            projectionMatrix
            geometryBufferData
            (_descriptorSets._descriptorSetData $ applicationData)
            (_sceneConstantsMemories applicationData)

    terminateApplication applicationData
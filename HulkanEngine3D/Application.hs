{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}

module HulkanEngine3D.Application
    ( InputData (..)
    , glfwMainLoop
    , createGLFWWindow
    , destroyGLFWWindow
    ) where


import Control.Monad
import Data.IORef
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger

data InputData = InputData
    {
    } deriving (Show)

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
    return ()
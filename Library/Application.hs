{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Application
    ( glfwMainLoop
    , createGLFWWindow
    ) where


import Control.Monad
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import Library.Utils


glfwMainLoop :: GLFW.Window -> IO () -> IO ()
glfwMainLoop window mainLoop = go
  where
    go = do
      should <- GLFW.windowShouldClose window
      unless should $ GLFW.pollEvents >> mainLoop >> go


createGLFWWindow::Int -> Int -> String -> IO (Maybe GLFW.Window)
createGLFWWindow width height title = do
  GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")  
  putStrLn "Initialized GLFW."
  version <- GLFW.getVersionString
  mapM_ (putStrLn . ("GLFW Version: " ++)) version
  GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable True
  window <- GLFW.createWindow width height title Nothing Nothing
  return window

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Application
    ( withGLFWWindow
    , glfwMainLoop ) where

import Control.Monad
import Lib.Utils
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW


glfwMainLoop :: GLFW.Window -> IO () -> IO ()
glfwMainLoop window mainLoop = go
  where
    go = do
      should <- GLFW.windowShouldClose window
      unless should $ GLFW.pollEvents >> mainLoop >> go


withGLFWWindow::IO (Maybe GLFW.Window)
withGLFWWindow = do
  GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")  
  putStrLn "Initialized GLFW."
  version <- GLFW.getVersionString
  mapM_ (putStrLn . ("GLFW Version: " ++)) version
  GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable True
  window <- GLFW.createWindow 800 600 "Vulkan Window" Nothing Nothing
  return window
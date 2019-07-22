{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Application
    ( glfwMainLoop
    , createGLFWWindow
    ) where


import Control.Monad
import Data.IORef
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import Library.Utils


glfwMainLoop :: GLFW.Window -> IO Bool -> IO ()
glfwMainLoop window mainLoop = go
  where
    go = do
      should <- GLFW.windowShouldClose window
      unless should $ do
        GLFW.pollEvents
        result <- mainLoop
        if result then go else return ()


createGLFWWindow::Int -> Int -> String -> IORef Bool -> IO (Maybe GLFW.Window)
createGLFWWindow width height title windowSizeChanged = do
  GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")  
  putStrLn "Initialized GLFW."
  version <- GLFW.getVersionString
  mapM_ (putStrLn . ("GLFW Version: " ++)) version
  GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable True
  maybeWindow <- GLFW.createWindow width height title Nothing Nothing
  let Just window = maybeWindow
  
  GLFW.setWindowSizeCallback window $ do
    Just (\_ _ _ -> atomicWriteIORef windowSizeChanged True)

  return maybeWindow

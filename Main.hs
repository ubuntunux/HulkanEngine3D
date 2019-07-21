{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Graphics.Vulkan.Core_1_0
import qualified Graphics.UI.GLFW as GLFW
import Library.Utils
import Library.Application
import Library.Vulkan
import qualified Library.Constants as Constants


main::IO()
main = do   
  maybeWindow <- createGLFWWindow 800 600 "Vulkan Application"
  when (Nothing == maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
  putStrLn "Initialized GLFW window."  
  requireExtensions <- GLFW.getRequiredInstanceExtensions
  instanceExtensionNames <- getInstanceExtensionSupport
  result <- checkExtensionSupport instanceExtensionNames requireExtensions
  unless result (throwVKMsg "Failed to initialize GLFW window.")  
  let
    Just window = maybeWindow
    progName = "02-GLFWWindow"
    engineName = "My perfect Haskell engine" 
    isConcurrentMode = True
  defaultRenderData <- getDefaultRenderData
  (renderData', swapChainSupportDetails) <- createRenderer defaultRenderData window progName engineName isConcurrentMode requireExtensions
  renderData <- createRenderData renderData' swapChainSupportDetails False
  frameIndexRef <- newIORef 0
  imageIndexPtr <- new 0
          
  -- Main Loop
  glfwMainLoop window $ do
    frameIndex <- readIORef frameIndexRef
    drawFrame renderData frameIndex imageIndexPtr
    writeIORef frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
    
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle (_device renderData)

  -- Terminate
  putStrLn "\n[ Terminate ]"
  destroyRenderer renderData  
  free imageIndexPtr
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


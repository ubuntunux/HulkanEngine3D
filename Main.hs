{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Lib.Utils
import qualified Graphics.UI.GLFW as GLFW
import Library.Vulkan
import Library.Application

mainLoop::IO()
mainLoop = do
  return ()

main::IO()
main = do 
  maybeWindow <- withGLFWWindow
  when (Nothing == maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
  putStrLn "Initialized GLFW window."
  glfwReqExts <- GLFW.getRequiredInstanceExtensions
  let
    Just window = maybeWindow
    progName = "02-GLFWWindow"
    engineName = "My perfect Haskell engine"
    extensions = glfwReqExts
    layers = ["VK_LAYER_LUNARG_standard_validation"]
  vkInstance <- createVulkanInstance progName engineName extensions layers
  (_, device) <- selectPhysicalDevice vkInstance Nothing
  putStrLn $ "Selected physical device: " ++ show device
  glfwMainLoop window mainLoop
  destroyVulkanInstance vkInstance >> putStrLn "Destroy VulkanInstance."
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


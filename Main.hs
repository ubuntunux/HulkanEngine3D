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
  maybeWindow <- createGLFWWindow 800 600 "Vulkan Application"
  when (Nothing == maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
  putStrLn "Initialized GLFW window."
  extensions <- GLFW.getRequiredInstanceExtensions  
  let
    Just window = maybeWindow
    progName = "02-GLFWWindow"
    engineName = "My perfect Haskell engine"
    layers = ["VK_LAYER_LUNARG_standard_validation"]
    applicationInfo = getApplicationInfo progName engineName
    instanceCreateInfo = getInstanceCreateInfo applicationInfo layers extensions
  vkInstance <- createVulkanInstance instanceCreateInfo  
  surface <- createSurface vkInstance window  
  (_, physical_device) <- selectPhysicalDevice vkInstance Nothing  
  putStrLn $ "Selected physical device: " ++ show physical_device
  (queueFamilyIndex, queueFamilyProperties) <- getQueueFamilyIndex physical_device
  physicalDeviceFeatures <- getPhysicalDeviceFeatures
  queueCreateInfo <- getQueueCreateInfo queueFamilyIndex
  deviceCreateInfo <- getDeviceCreateInfo queueCreateInfo physicalDeviceFeatures layers
  (device, queue) <- createGraphicsDevice deviceCreateInfo physical_device queueFamilyIndex  
  glfwMainLoop window mainLoop
  destroyDevice device
  destroySurface vkInstance surface
  destroyVulkanInstance vkInstance >> putStrLn "Destroy VulkanInstance."
  touchVKDatas instanceCreateInfo deviceCreateInfo physicalDeviceFeatures queueCreateInfo
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


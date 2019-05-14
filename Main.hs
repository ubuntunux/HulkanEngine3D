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
import Foreign.C.String
import Foreign.Ptr
mainLoop::IO()
mainLoop = do
  return ()

main::IO()
main = do 
  maybeWindow <- createGLFWWindow 800 600 "Vulkan Application"
  when (Nothing == maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
  putStrLn "Initialized GLFW window."
  requireExtensions <- GLFW.getRequiredInstanceExtensions
  instanceExtensionNames <- getInstanceExtensionSupport
  checkExtensionSupport instanceExtensionNames requireExtensions  
  let
    Just window = maybeWindow
    progName = "02-GLFWWindow"
    engineName = "My perfect Haskell engine"
    layers = ["VK_LAYER_LUNARG_standard_validation"]
    applicationInfo = getApplicationInfo progName engineName
    instanceCreateInfo = getInstanceCreateInfo applicationInfo layers requireExtensions
  vkInstance <- createVulkanInstance instanceCreateInfo  
  vkSurface <- createVkSurface vkInstance window
  putStrLn $ "Createad surface: " ++ show vkSurface
  (Just swapChainSupportDetails, physicalDevice) <- selectPhysicalDevice vkInstance (Just vkSurface)
  queueFaimilies <- getQueueFamilies physicalDevice  
  let (graphicsQueueFamilyIndex, graphicsQueueFamilyProperties) = selectGraphicsFamily queueFaimilies
  let (transferQueueFamilyIndex, transferQueueFamilyProperties) = selectTransferFamily queueFaimilies
  let (computeQueueFamilyIndex, computeQueueFamilyProperties) = selectComputeFamily queueFaimilies
  (presentationFamilyIndex, presentationFamilyProperties) <- selectPresentationFamily physicalDevice vkSurface queueFaimilies
  physicalDeviceFeatures <- getPhysicalDeviceFeatures
  queueCreateInfo <- getQueueCreateInfo graphicsQueueFamilyIndex
  deviceCreateInfo <- getDeviceCreateInfo queueCreateInfo physicalDeviceFeatures layers
  (device, graphicsQueue) <- createGraphicsDevice deviceCreateInfo physicalDevice graphicsQueueFamilyIndex  
  glfwMainLoop window mainLoop
  destroyDevice device
  destroySurface vkInstance vkSurface
  destroyVulkanInstance vkInstance >> putStrLn "Destroy VulkanInstance."
  touchVKDatas instanceCreateInfo deviceCreateInfo physicalDeviceFeatures queueCreateInfo
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


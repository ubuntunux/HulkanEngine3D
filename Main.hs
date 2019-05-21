{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import qualified Data.Map as Map
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr
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
  physicalDeviceFeatures <- getPhysicalDeviceFeatures  
  (queueFamilyIndices, queueFamilyPropertiese) <- getQueueFamilyInfos physicalDevice vkSurface
  queuePrioritiesPtr <- getQueuePrioritiesPtr 1.0
  queueCreateInfoList <- getQueueCreateInfos queueFamilyIndices queuePrioritiesPtr  
  queueCreateInfoArrayPtr <- newArray queueCreateInfoList
  requireDeviceExtensionsPtr <- newArray requireDeviceExtensions
  deviceCreateInfo <- getDeviceCreateInfo 
    queueCreateInfoArrayPtr 
    (length queueCreateInfoList)
    physicalDeviceFeatures
    layers
    (length requireDeviceExtensions)
    requireDeviceExtensionsPtr
  logicalDevice <- createDevice deviceCreateInfo physicalDevice 
  queueList <- createQueues logicalDevice queueFamilyIndices
  glfwMainLoop window mainLoop
  destroyDevice logicalDevice
  destroySurface vkInstance vkSurface
  destroyVulkanInstance vkInstance >> putStrLn "Destroy VulkanInstance."
  touchVKDatas instanceCreateInfo deviceCreateInfo physicalDeviceFeatures  
  free requireDeviceExtensionsPtr
  free queueCreateInfoArrayPtr
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


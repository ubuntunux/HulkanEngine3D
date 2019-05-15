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
  queueFamilyMap <- getQueueFamilyMap physicalDevice vkSurface
  queuePrioritiesPtr <- getQueuePrioritiesPtr 1.0
  
  -- 임시 -- 없애자
  queueFaimilies <- getQueueFamilies physicalDevice
  (graphicsQueueFamilyIndex, graphicsQueueFamilyProperties) <- selectGraphicsFamily queueFaimilies
  -- 임시 -- 없애자

  queueCreateInfoMap <- getQueueCreateInfo queueFamilyMap queuePrioritiesPtr
  queueCreateInfoArrayPtr <- newArray $ Map.elems queueCreateInfoMap
  deviceCreateInfo <- getDeviceCreateInfo queueCreateInfoArrayPtr physicalDeviceFeatures layers
  (device, graphicsQueue) <- createGraphicsDevice deviceCreateInfo physicalDevice graphicsQueueFamilyIndex  
  glfwMainLoop window mainLoop
  destroyDevice device
  destroySurface vkInstance vkSurface
  destroyVulkanInstance vkInstance >> putStrLn "Destroy VulkanInstance."
  touchVKDatas instanceCreateInfo deviceCreateInfo physicalDeviceFeatures
  free queueCreateInfoArrayPtr
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


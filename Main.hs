{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr
import Lib.Utils
import Graphics.Vulkan
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
  (instanceCreateInfo, vkInstance) <- createVulkanInstance progName engineName vulkanLayers requireExtensions
  vkSurface <- createVkSurface vkInstance window
  (Just swapChainSupportDetails, physicalDevice) <- selectPhysicalDevice vkInstance (Just vkSurface)  
  physicalDeviceFeatures <- getPhysicalDeviceFeatures  
  queueFamilyIndices <- getQueueFamilyIndices physicalDevice vkSurface
  let 
    graphicsQueueIndex' = graphicsQueueIndex queueFamilyIndices
    presentQueueIndex' = presentQueueIndex queueFamilyIndices
    createQueueFamilyMap = Map.fromList $ [(index, index) | index <- [graphicsQueueIndex', presentQueueIndex']]
  queuePrioritiesPtr <- getQueuePrioritiesPtr 1.0
  queueCreateInfoList <- getQueueCreateInfos (Map.elems createQueueFamilyMap) queuePrioritiesPtr  
  queueCreateInfoArrayPtr <- newArray queueCreateInfoList
  requireDeviceExtensionsPtr <- newArray requireDeviceExtensions
  deviceCreateInfo <- getDeviceCreateInfo 
    queueCreateInfoArrayPtr 
    (length queueCreateInfoList)
    physicalDeviceFeatures
    vulkanLayers
    (length requireDeviceExtensions)
    requireDeviceExtensionsPtr
  vkDevice <- createDevice deviceCreateInfo physicalDevice 
  queueMap <- createQueues vkDevice (Map.elems createQueueFamilyMap)
  createQueueFamilyListPtr <- newArray (Map.elems createQueueFamilyMap)
  let 
    defaultQueue = (Map.elems queueMap) !! 0
    queueFamilyDatas = QueueFamilyDatas
        { graphicsQueue = fromMaybe defaultQueue $ Map.lookup graphicsQueueIndex' queueMap
        , presentQueue = fromMaybe defaultQueue $ Map.lookup presentQueueIndex' queueMap
        , queueFamilyCount = fromIntegral $ length queueMap
        , queueFamilyIndicesPtr = createQueueFamilyListPtr
        , graphicsFamilyIndex = graphicsQueueIndex'
        , presentFamilyIndex = presentQueueIndex' }
    imageCount = 3 -- tripple buffering
  (swapChainCreateInfo, swapChainImageDatas) <- createSwapChain vkDevice swapChainSupportDetails imageCount queueFamilyDatas vkSurface
  (swapChainImageViewCreateInfos, swapChainImageViews) <- createSwapChainImageViews vkDevice swapChainImageDatas
  glfwMainLoop window mainLoop
  destroySwapChainImageViews vkDevice swapChainImageViewCreateInfos swapChainImageViews
  destroySwapChain vkDevice swapChainCreateInfo (swapChain swapChainImageDatas)
  destroyDevice vkDevice deviceCreateInfo physicalDeviceFeatures
  destroyVkSurface vkInstance vkSurface
  destroyVulkanInstance instanceCreateInfo vkInstance
  free requireDeviceExtensionsPtr
  free queueCreateInfoArrayPtr
  free createQueueFamilyListPtr  
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


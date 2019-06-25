{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
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
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import qualified Graphics.UI.GLFW as GLFW
import Lib.Utils
import Library.Application
import Library.Shader
import Library.Vulkan


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
    isConcurrentMode = True
  vkInstance <- createVulkanInstance progName engineName vulkanLayers requireExtensions
  vkSurface <- createVkSurface vkInstance window
  (Just swapChainSupportDetails, physicalDevice) <- selectPhysicalDevice vkInstance (Just vkSurface)  
  physicalDeviceFeatures <- getPhysicalDeviceFeatures  
  queueFamilyIndices <- getQueueFamilyIndices physicalDevice vkSurface isConcurrentMode
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
  device <- createDevice deviceCreateInfo physicalDevice 
  queueMap <- createQueues device (Map.elems createQueueFamilyMap)
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
  swapChainData <- createSwapChain device swapChainSupportDetails imageCount queueFamilyDatas vkSurface
  swapChainImageViews <- createSwapChainImageViews device swapChainData
  vertexShaderCreateInfo <- createShaderStageCreateInfo device "shaders/triangle.vert" VK_SHADER_STAGE_VERTEX_BIT
  fragmentShaderCreateInfo <- createShaderStageCreateInfo device "shaders/triangle.frag" VK_SHADER_STAGE_FRAGMENT_BIT
  renderPass <- createRenderPass device swapChainData
  pipelineLayout <- createPipelineLayout device  
  graphicsPipeline <- createGraphicsPipeline device swapChainData [vertexShaderCreateInfo, fragmentShaderCreateInfo] renderPass pipelineLayout
  frameBuffers <- createFramebuffers device renderPass swapChainData swapChainImageViews
  commandPool <- createCommandPool device queueFamilyDatas
  (commandBuffers, commandBuffersPtr) <- createCommandBuffers device graphicsPipeline commandPool renderPass swapChainData frameBuffers
  imageAvailableSemaphore <- createSemaphore device
  renderFinishedSemaphore <- createSemaphore device  

  renderData <- alloca $ \imageIndexPtr -> do
    return RenderData
          { imageAvailableSemaphore = imageAvailableSemaphore
          , renderFinishedSemaphore = renderFinishedSemaphore
          , device = device
          , swapChainData = swapChainData
          , queueFamilyDatas = queueFamilyDatas
          , imageIndexPtr = imageIndexPtr
          , commandBuffers = commandBuffers }

  -- Main Loop
  glfwMainLoop window $ do    
    drawFrame renderData
    
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle device

  -- Terminate
  putStrLn "\n[ Terminate ]"
  destroySemaphore device renderFinishedSemaphore
  destroySemaphore device imageAvailableSemaphore  
  destroyCommandBuffers device commandPool (fromIntegral $ length commandBuffers) commandBuffersPtr
  destroyCommandPool device commandPool
  destroyFramebuffers device frameBuffers
  destroyGraphicsPipeline device graphicsPipeline
  destroyPipelineLayout device pipelineLayout
  destroyRenderPass device renderPass
  destroyShaderStageCreateInfo device vertexShaderCreateInfo
  destroyShaderStageCreateInfo device fragmentShaderCreateInfo
  destroySwapChainImageViews device swapChainImageViews
  destroySwapChain device (swapChain swapChainData)
  destroyDevice device deviceCreateInfo physicalDeviceFeatures
  destroyVkSurface vkInstance vkSurface
  destroyVulkanInstance vkInstance
  free requireDeviceExtensionsPtr
  free queueCreateInfoArrayPtr
  free createQueueFamilyListPtr  
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


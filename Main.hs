{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Graphics.Vulkan.Core_1_0
import qualified Graphics.UI.GLFW as GLFW
import Library.Utils
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
  result <- checkExtensionSupport instanceExtensionNames requireExtensions
  unless result (throwVKMsg "Failed to initialize GLFW window.")  
  let
    Just window = maybeWindow
    progName = "02-GLFWWindow"
    engineName = "My perfect Haskell engine"
    isConcurrentMode = True
  vkInstance <- createVulkanInstance progName engineName vulkanLayers requireExtensions
  vkSurface <- createVkSurface vkInstance window
  (Just swapChainSupportDetails, physicalDevice) <- selectPhysicalDevice vkInstance (Just vkSurface)  
  deviceProperties <- getPhysicalDeviceProperties physicalDevice
  msaaSamples <- getMaxUsableSampleCount deviceProperties  
  queueFamilyIndices <- getQueueFamilyIndices physicalDevice vkSurface isConcurrentMode
  let 
    graphicsQueueIndex' = graphicsQueueIndex queueFamilyIndices
    presentQueueIndex' = presentQueueIndex queueFamilyIndices    
    queueFamilyIndexList = Set.toList $ Set.fromList [graphicsQueueIndex', presentQueueIndex']  
  devicePtr <- createDevice physicalDevice queueFamilyIndexList
  device <- peek devicePtr
  queueMap <- createQueues device queueFamilyIndexList
  queueFamilyIndicesPtr <- newArray queueFamilyIndexList
  let 
    defaultQueue = (Map.elems queueMap) !! 0
    queueFamilyDatas = QueueFamilyDatas
        { graphicsQueue = fromMaybe defaultQueue $ Map.lookup graphicsQueueIndex' queueMap
        , presentQueue = fromMaybe defaultQueue $ Map.lookup presentQueueIndex' queueMap
        , queueFamilyCount = fromIntegral $ length queueMap
        , queueFamilyIndicesPtr = queueFamilyIndicesPtr
        , graphicsFamilyIndex = graphicsQueueIndex'
        , presentFamilyIndex = presentQueueIndex' }
    imageCount = 3 -- tripple buffering
  swapChainData <- createSwapChain device swapChainSupportDetails imageCount queueFamilyDatas vkSurface
  swapChainImageViews <- createSwapChainImageViews device swapChainData
  vertexShaderCreateInfo <- createShaderStageCreateInfo device "Resource/Shaders/triangle.vert" VK_SHADER_STAGE_VERTEX_BIT
  fragmentShaderCreateInfo <- createShaderStageCreateInfo device "Resource/Shaders/triangle.frag" VK_SHADER_STAGE_FRAGMENT_BIT
  renderPass <- createRenderPass device swapChainData
  pipelineLayout <- createPipelineLayout device  
  graphicsPipeline <- createGraphicsPipeline device swapChainData [vertexShaderCreateInfo, fragmentShaderCreateInfo] renderPass pipelineLayout
  frameBuffers <- createFramebuffers device renderPass swapChainData swapChainImageViews
  commandPool <- createCommandPool device queueFamilyDatas
  (commandBuffersPtr, commandBufferCount) <- createCommandBuffers device graphicsPipeline commandPool renderPass swapChainData frameBuffers
  imageAvailableSemaphores <- createSemaphores device
  renderFinishedSemaphores <- createSemaphores device  
  frameFencesPtr <- createFrameFences device
  frameIndexRef <- newIORef 0
  imageIndexPtr <- new 0
  renderData <- pure RenderData
          { frameIndexRef = frameIndexRef
          , imageAvailableSemaphores = imageAvailableSemaphores
          , renderFinishedSemaphores = renderFinishedSemaphores
          , device = device
          , swapChainData = swapChainData
          , queueFamilyDatas = queueFamilyDatas
          , imageIndexPtr = imageIndexPtr
          , commandBuffersPtr = commandBuffersPtr
          , frameFencesPtr = frameFencesPtr }

  -- Main Loop
  glfwMainLoop window $ do    
    drawFrame renderData
    
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle device

  -- Terminate
  putStrLn "\n[ Terminate ]"

  cleanupSwapChain device frameBuffers commandPool commandBufferCount commandBuffersPtr graphicsPipeline pipelineLayout renderPass swapChainImageViews (swapChain swapChainData)

  destroyFrameFences device frameFencesPtr
  destroySemaphores device renderFinishedSemaphores
  destroySemaphores device imageAvailableSemaphores 
  --destroyCommandBuffers device commandPool (fromIntegral commandBufferCount) commandBuffersPtr
  destroyCommandPool device commandPool
  --destroyFramebuffers device frameBuffers
  --destroyGraphicsPipeline device graphicsPipeline
  --destroyPipelineLayout device pipelineLayout
  --destroyRenderPass device renderPass
  destroyShaderStageCreateInfo device vertexShaderCreateInfo
  destroyShaderStageCreateInfo device fragmentShaderCreateInfo
  --destroySwapChainImageViews device swapChainImageViews
  --destroySwapChain device (swapChain swapChainData)  

  destroyDevice devicePtr
  destroyVkSurface vkInstance vkSurface
  destroyVulkanInstance vkInstance
  
  free queueFamilyIndicesPtr  
  free commandBuffersPtr
  free frameFencesPtr
  free imageIndexPtr

  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


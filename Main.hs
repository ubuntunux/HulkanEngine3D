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
  let graphicsQueueIndex = _graphicsQueueIndex queueFamilyIndices
      presentQueueIndex = _presentQueueIndex queueFamilyIndices    
      queueFamilyIndexList = Set.toList $ Set.fromList [graphicsQueueIndex, presentQueueIndex]  
  devicePtr <- createDevice physicalDevice queueFamilyIndexList
  device <- peek devicePtr
  queueMap <- createQueues device queueFamilyIndexList
  queueFamilyIndicesPtr <- newArray queueFamilyIndexList
  let defaultQueue = (Map.elems queueMap) !! 0
      queueFamilyDatas = QueueFamilyDatas
          { _graphicsQueue = fromMaybe defaultQueue $ Map.lookup graphicsQueueIndex queueMap
          , _presentQueue = fromMaybe defaultQueue $ Map.lookup presentQueueIndex queueMap
          , _queueFamilyCount = fromIntegral $ length queueMap
          , _queueFamilyIndicesPtr = queueFamilyIndicesPtr
          , _graphicsFamilyIndex = graphicsQueueIndex
          , _presentFamilyIndex = presentQueueIndex }
      imageCount = 3 -- tripple buffering
  swapChainData <- createSwapChain device swapChainSupportDetails imageCount queueFamilyDatas vkSurface  
  vertexShaderCreateInfo <- createShaderStageCreateInfo device "Resource/Shaders/triangle.vert" VK_SHADER_STAGE_VERTEX_BIT
  fragmentShaderCreateInfo <- createShaderStageCreateInfo device "Resource/Shaders/triangle.frag" VK_SHADER_STAGE_FRAGMENT_BIT
  renderPass <- createRenderPass device swapChainData
  pipelineLayout <- createPipelineLayout device  
  graphicsPipeline <- createGraphicsPipeline device swapChainData [vertexShaderCreateInfo, fragmentShaderCreateInfo] renderPass pipelineLayout
  frameBuffers <- createFramebuffers device renderPass swapChainData
  commandPool <- createCommandPool device queueFamilyDatas
  (commandBuffersPtr, commandBufferCount) <- createCommandBuffers device graphicsPipeline commandPool renderPass swapChainData frameBuffers
  imageAvailableSemaphores <- createSemaphores device
  renderFinishedSemaphores <- createSemaphores device  
  frameFencesPtr <- createFrameFences device
  frameIndexRef <- newIORef 0
  imageIndexPtr <- new 0
  let renderData = RenderData
          { _frameIndexRef = frameIndexRef
          , _msaaSamples = msaaSamples
          , _imageAvailableSemaphores = imageAvailableSemaphores
          , _renderFinishedSemaphores = renderFinishedSemaphores
          , _device = device
          , _swapChainData = swapChainData
          , _queueFamilyDatas = queueFamilyDatas
          , _imageIndexPtr = imageIndexPtr
          , _frameFencesPtr = frameFencesPtr
          , _frameBuffers = frameBuffers
          , _commandPool = commandPool
          , _commandBufferCount = (fromIntegral commandBufferCount)
          , _commandBuffersPtr = commandBuffersPtr
          , _graphicsPipeline = graphicsPipeline
          , _pipelineLayout = pipelineLayout
          , _renderPass = renderPass
          }

  -- Main Loop
  glfwMainLoop window $ do    
    drawFrame renderData
    
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle device

  -- Terminate
  putStrLn "\n[ Terminate ]"

  cleanupSwapChain device frameBuffers commandPool commandBufferCount commandBuffersPtr graphicsPipeline pipelineLayout renderPass (_swapChainImageViews swapChainData) (_swapChain swapChainData)

  destroyShaderStageCreateInfo device vertexShaderCreateInfo
  destroyShaderStageCreateInfo device fragmentShaderCreateInfo

  destroySemaphores device renderFinishedSemaphores
  destroySemaphores device imageAvailableSemaphores 
  destroyFrameFences device frameFencesPtr  
  destroyCommandPool device commandPool
  destroyDevice devicePtr
  destroyVkSurface vkInstance vkSurface
  destroyVulkanInstance vkInstance
  
  free queueFamilyIndicesPtr      
  free imageIndexPtr

  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


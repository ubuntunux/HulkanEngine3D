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
import Foreign.Marshal.Utils
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
  device <- createDevice physicalDevice queueFamilyIndexList
  queueMap <- createQueues device queueFamilyIndexList  
  let defaultQueue = (Map.elems queueMap) !! 0
      queueFamilyDatas = QueueFamilyDatas
          { _graphicsQueue = fromMaybe defaultQueue $ Map.lookup graphicsQueueIndex queueMap
          , _presentQueue = fromMaybe defaultQueue $ Map.lookup presentQueueIndex queueMap
          , _queueFamilyCount = fromIntegral $ length queueMap
          , _queueFamilyIndices = queueFamilyIndices
          }
      imageCount = 3 -- tripple buffering
  swapChainData <- createSwapChain device swapChainSupportDetails imageCount queueFamilyDatas queueFamilyIndexList vkSurface
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
          , _vkInstance = vkInstance
          , _vkSurface = vkSurface
          , _device = device
          , _swapChainData = swapChainData
          , _queueFamilyDatas = queueFamilyDatas
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
    drawFrame renderData imageIndexPtr
    
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle device

  -- Terminate
  putStrLn "\n[ Terminate ]"
  destroyShaderStageCreateInfo device vertexShaderCreateInfo
  destroyShaderStageCreateInfo device fragmentShaderCreateInfo
  cleanup renderData  
  free imageIndexPtr
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


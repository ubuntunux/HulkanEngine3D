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
import Library.Vulkan
import qualified Library.Constants as Constants


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
  vkInstance <- createVulkanInstance progName engineName Constants.vulkanLayers requireExtensions
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
          , _queueFamilyIndexList = queueFamilyIndexList
          , _queueFamilyCount = fromIntegral $ length queueMap
          , _queueFamilyIndices = queueFamilyIndices }      
  swapChainData <- createSwapChain device swapChainSupportDetails queueFamilyDatas vkSurface
  renderPass <- createRenderPass device swapChainData  
  let vertexShaderFile = "Resource/Shaders/triangle.vert"
      fragmentShaderFile = "Resource/Shaders/triangle.frag"
  graphicsPipelineData <- createGraphicsPipeline device (_swapChainExtent swapChainData) vertexShaderFile fragmentShaderFile renderPass
  frameBuffers <- createFramebuffers device renderPass swapChainData
  commandPool <- createCommandPool device queueFamilyDatas
  (commandBuffersPtr, commandBufferCount) <- createCommandBuffers device (_pipeline graphicsPipelineData) commandPool renderPass swapChainData frameBuffers
  imageAvailableSemaphores <- createSemaphores device
  renderFinishedSemaphores <- createSemaphores device  
  frameFencesPtr <- createFrameFences device
  frameIndexRef <- newIORef 0
  imageIndexPtr <- new 0
  renderData <- pure RenderData
      { _msaaSamples = msaaSamples
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
      , _graphicsPipelineData = graphicsPipelineData
      , _renderPass = renderPass }
          
  -- Main Loop
  glfwMainLoop window $ do
    frameIndex <- readIORef frameIndexRef
    drawFrame renderData frameIndex imageIndexPtr
    writeIORef frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
    
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle device

  -- Terminate
  putStrLn "\n[ Terminate ]"  
  cleanup renderData  
  free imageIndexPtr
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


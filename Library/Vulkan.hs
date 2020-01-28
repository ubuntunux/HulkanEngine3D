{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
  ( RendererData (..)
  , getMaxUsableSampleCount
  , getInstanceExtensionSupport
  , getDeviceExtensionSupport
  , checkExtensionSupport
  , createVulkanInstance
  , destroyVulkanInstance
  , createVkSurface
  , destroyVkSurface
  , selectPhysicalDevice
  , getPhysicalDeviceProperties
  , createDevice
  , destroyDevice
  , createGraphicsRenderPass
  , destroyGraphicsRenderPass
  , createCommandPool
  , destroyCommandPool
  , createCommandBuffers
  , destroyCommandBuffers
  , recordCommandBuffer
  , createSemaphores
  , destroySemaphores
  , createFrameFences
  , destroyFrameFences
  , drawFrame
  , getDefaultRendererData
  , createRenderer
  , destroyRenderer
  , recreateSwapChain
  , runCommandsOnce
  ) where

import Control.Monad
import Data.Bits
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import qualified Graphics.UI.GLFW as GLFW

import qualified Library.Constants as Constants
import Library.Utils
import Library.Logger
import Library.Vulkan.FrameBuffer
import Library.Vulkan.RenderPass
import Library.Vulkan.Queue
import Library.Vulkan.SwapChain


data RendererData = RendererData
    { _msaaSamples :: VkSampleCountBitmask FlagBit
    , _imageAvailableSemaphores :: [VkSemaphore]
    , _renderFinishedSemaphores :: [VkSemaphore]
    , _vkInstance :: VkInstance
    , _vkSurface :: VkSurfaceKHR
    , _device :: VkDevice
    , _physicalDevice :: VkPhysicalDevice
    , _swapChainData :: SwapChainData
    , _swapChainSupportDetails :: SwapChainSupportDetails
    , _queueFamilyDatas :: QueueFamilyDatas
    , _frameFencesPtr :: Ptr VkFence
    , _commandPool :: VkCommandPool
    , _commandBufferCount :: Word32
    , _commandBuffersPtr :: Ptr VkCommandBuffer
    } deriving (Eq, Show)


getExtensionNames :: (Traversable t1, VulkanMarshal t) => [Char] -> t1 t -> IO (t1 String)
getExtensionNames extensionType availableExtensionArrayPtr = do
  availableExtensionNames <- mapM getExtensionName availableExtensionArrayPtr
  logInfo $ "Available " ++ extensionType ++ " extensions : " ++ (show (length availableExtensionNames))
  --mapM (\extensionName -> logInfo $ "    " ++ extensionName) availableExtensionNames
  return availableExtensionNames
  where 
    getExtensionName extensionPtr = 
      let extensionNamePtr = plusPtr (unsafePtr extensionPtr) (fieldOffset @"extensionName" @VkExtensionProperties)
      in peekCString $ castPtr extensionNamePtr

getInstanceExtensionSupport :: IO [String]
getInstanceExtensionSupport = do
  availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> do
      result <- vkEnumerateInstanceExtensionProperties VK_NULL_HANDLE counterPtr valueArrayPtr
      validationVK result "vkEnumerateInstanceExtensionProperties error"
  getExtensionNames "Instance" availableExtensionArrayPtr

getDeviceExtensionSupport :: VkPhysicalDevice -> IO [String]
getDeviceExtensionSupport physicalDevice = do
  availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> do
      result <- vkEnumerateDeviceExtensionProperties physicalDevice VK_NULL_HANDLE counterPtr valueArrayPtr
      validationVK result "vkEnumerateInstanceExtensionProperties error"
  getExtensionNames "Device" availableExtensionArrayPtr

checkExtensionSupport :: [String] -> [CString] -> IO Bool
checkExtensionSupport availableDeviceExtensions requireExtensions = do
  requireExtensionNames <- mapM peekCString requireExtensions    
  logInfo $ "Require Extensions: " ++ show (length requireExtensionNames) ++ " / " ++ show (length availableDeviceExtensions) ++ " availables."
  isAvailable requireExtensionNames
  return . null $ requireExtensionNames \\ availableDeviceExtensions
  where 
    isAvailable [] = return ()
    isAvailable (x:xs) = do
      if elem x availableDeviceExtensions
        then logInfo ("    " ++ x ++ " (OK)")
        else logInfo ("    " ++ x ++ " (Failed)")
      isAvailable xs

createVulkanInstance :: String -> String -> [String] -> [CString] -> IO VkInstance
createVulkanInstance progName engineName layers extensions = do
  let 
    applicationInfo :: VkApplicationInfo
    applicationInfo = createVk @VkApplicationInfo
      $ set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      &* set @"pNext" VK_NULL
      &* setStrRef @"pApplicationName" progName
      &* set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
      &* setStrRef @"pEngineName" engineName
      &* set @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
      &* set @"apiVersion" (_VK_MAKE_VERSION 1 0 0)
    instanceCreateInfo :: VkInstanceCreateInfo
    instanceCreateInfo = createVk @VkInstanceCreateInfo
      $ set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* setVkRef @"pApplicationInfo" applicationInfo
      &* set @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef @"ppEnabledExtensionNames" extensions
  vkInstance <- alloca $ \vkInstPtr -> do
      result <- vkCreateInstance (unsafePtr instanceCreateInfo) VK_NULL vkInstPtr
      validationVK result "vkCreateInstance: Failed to create vkInstance."
      peek vkInstPtr
  touchVkData instanceCreateInfo  
  return vkInstance

destroyVulkanInstance :: VkInstance -> IO ()
destroyVulkanInstance vkInstance = do  
  logInfo "Destroy VulkanInstance"
  vkDestroyInstance vkInstance VK_NULL  

createVkSurface :: Ptr vkInstance -> GLFW.Window -> IO VkSurfaceKHR
createVkSurface vkInstance window = do
  vkSurface <- alloca $ \vkSurfacePtr -> do
    result <- GLFW.createWindowSurface vkInstance window VK_NULL_HANDLE vkSurfacePtr
    validationVK result "glfwCreateWindowSurface: failed to create window surface"
    logInfo $ "Createad surface: " ++ show vkSurfacePtr
    peek vkSurfacePtr    
  return vkSurface

destroyVkSurface :: VkInstance -> VkSurfaceKHR -> IO ()
destroyVkSurface vkInstance vkSurface = do
  destroySurfaceFunc <- vkGetInstanceProc @VkDestroySurfaceKHR vkInstance
  destroySurfaceFunc vkInstance vkSurface VK_NULL_HANDLE
  logInfo "Destroy VkSurfaceKHR"

querySwapChainSupport :: VkPhysicalDevice -> VkSurfaceKHR -> IO SwapChainSupportDetails
querySwapChainSupport physicalDevice vkSurface = do
  capabilities <- newVkData $ \pSurfaceCapabilities -> do
    result <- vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice vkSurface pSurfaceCapabilities
    validationVK result "vkGetPhysicalDeviceSurfaceCapabilitiesKHR error"
  formats <- asListVK $ \counterPtr valuePtr -> do
    result <- vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice vkSurface counterPtr valuePtr
    validationVK result "vkGetPhysicalDeviceSurfaceFormatsKHR error"
  presentModes <- asListVK $ \counterPtr valuePtr -> do
    result <- vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice vkSurface counterPtr valuePtr
    validationVK result "vkGetPhysicalDeviceSurfacePresentModesKHR error"
  return SwapChainSupportDetails { _capabilities = capabilities
                                 , _formats = formats
                                 , _presentModes = presentModes }

isDeviceSuitable :: Maybe VkSurfaceKHR -> VkPhysicalDevice -> IO (Maybe SwapChainSupportDetails, Bool)
isDeviceSuitable maybeVkSurface physicalDevice = do
  deviceExtensionNames <- getDeviceExtensionSupport physicalDevice
  hasExtension <- checkExtensionSupport deviceExtensionNames Constants.requireDeviceExtensions
  (maybeSwapChainSupportDetails, result) <- case maybeVkSurface of
    Nothing -> pure (Nothing, True)
    Just vkSurface
      | not hasExtension -> pure (Nothing, False)
      | otherwise -> do
        swapChainSupportDetails@SwapChainSupportDetails {..} <- querySwapChainSupport physicalDevice vkSurface
        return (Just swapChainSupportDetails, not (null _formats) && not (null _presentModes))
  pure (maybeSwapChainSupportDetails, hasExtension && result)

getPhysicalDeviceProperties :: VkPhysicalDevice -> IO VkPhysicalDeviceProperties
getPhysicalDeviceProperties physicalDevice = do
  deviceProperties <- alloca $ \propertiesPtr -> do
    vkGetPhysicalDeviceProperties physicalDevice propertiesPtr
    peek propertiesPtr
  return deviceProperties

selectPhysicalDevice :: VkInstance 
                     -> Maybe VkSurfaceKHR 
                     -> IO (Maybe SwapChainSupportDetails, VkPhysicalDevice)
selectPhysicalDevice vkInstance maybeVkSurface = do
  devices <- asListVK $ \counterPtr valueArrayPtr -> do
      result <- vkEnumeratePhysicalDevices vkInstance counterPtr valueArrayPtr
      validationVK result "pickPhysicalDevice: Failed to enumerate physical devices."
  when (null devices) $ throwVKMsg "Zeo device count!"
  logInfo $ "Found " ++ show (length devices) ++ " devices."
  selectFirstSuitable devices
  where
    selectFirstSuitable [] = throwVKMsg "No suitable devices!"
    selectFirstSuitable (physicalDevice:physicalDeviceArray) = do
      (maybeSwapChainSupportDetails, result) <- isDeviceSuitable maybeVkSurface physicalDevice
      if result then do
          logInfo $ "Selected physical device: " ++ show physicalDevice
          pure (maybeSwapChainSupportDetails, physicalDevice)
        else
          selectFirstSuitable physicalDeviceArray

createDevice :: VkPhysicalDevice -> [Word32] -> IO VkDevice
createDevice physicalDevice queueFamilyList = do  
  queuePrioritiesPtr <- new 1.0
  queueCreateInfoList <- forM queueFamilyList $ \queueFamilyIndex ->
    newVkData @VkDeviceQueueCreateInfo $ \queueCreateInfoPtr -> do
        writeField @"sType" queueCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        writeField @"pNext" queueCreateInfoPtr VK_NULL_HANDLE
        writeField @"flags" queueCreateInfoPtr VK_ZERO_FLAGS
        writeField @"queueFamilyIndex" queueCreateInfoPtr queueFamilyIndex
        writeField @"queueCount" queueCreateInfoPtr 1
        writeField @"pQueuePriorities" queueCreateInfoPtr queuePrioritiesPtr
  physicalDeviceFeatures <- newVkData @VkPhysicalDeviceFeatures clearStorable
  queueCreateInfoArrayPtr <- newArray queueCreateInfoList
  requireDeviceExtensionsPtr <- newArray Constants.requireDeviceExtensions
  deviceCreateInfo <- withCStringList Constants.vulkanLayers $ \layerCount layerNames -> do
    newVkData @VkDeviceCreateInfo $ \devCreateInfoPtr -> do
      writeField @"sType" devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
      writeField @"pNext" devCreateInfoPtr VK_NULL_HANDLE
      writeField @"flags" devCreateInfoPtr VK_ZERO_FLAGS
      writeField @"pQueueCreateInfos" devCreateInfoPtr queueCreateInfoArrayPtr
      writeField @"queueCreateInfoCount" devCreateInfoPtr (fromIntegral $ length queueCreateInfoList)
      writeField @"enabledLayerCount" devCreateInfoPtr (fromIntegral layerCount)
      writeField @"ppEnabledLayerNames" devCreateInfoPtr layerNames
      writeField @"enabledExtensionCount" devCreateInfoPtr (fromIntegral $ length Constants.requireDeviceExtensions)
      writeField @"ppEnabledExtensionNames" devCreateInfoPtr requireDeviceExtensionsPtr
      writeField @"pEnabledFeatures" devCreateInfoPtr (unsafePtr physicalDeviceFeatures)
  device <- alloca $ \devicePtr -> do
    result <- vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE devicePtr
    validationVK result "vkCreateDevice: failed to create vkDevice"
    peek devicePtr
  logInfo $ "Created Device: " ++ show device
  touchVkData deviceCreateInfo
  touchVkData physicalDeviceFeatures
  free requireDeviceExtensionsPtr
  free queueCreateInfoArrayPtr
  free queuePrioritiesPtr
  return device

destroyDevice :: VkDevice -> IO ()
destroyDevice device = do
  vkDestroyDevice device VK_NULL_HANDLE
  logInfo "Destroy VkDevice"

getMaxUsableSampleCount :: VkPhysicalDeviceProperties -> IO VkSampleCountFlagBits
getMaxUsableSampleCount deviceProperties = do
  let limits = getField @"limits" deviceProperties
      colorSampleCounts = getField @"framebufferColorSampleCounts" limits
      depthSampleCounts = getField @"framebufferDepthSampleCounts" limits
      counts = min colorSampleCounts depthSampleCounts
      splitCounts = filter ((/= VK_ZERO_FLAGS) . (counts .&.))
        [ VK_SAMPLE_COUNT_64_BIT
        , VK_SAMPLE_COUNT_32_BIT
        , VK_SAMPLE_COUNT_16_BIT
        , VK_SAMPLE_COUNT_8_BIT
        , VK_SAMPLE_COUNT_4_BIT
        , VK_SAMPLE_COUNT_2_BIT
        , VK_SAMPLE_COUNT_1_BIT
        ]
      highestCount = head $ splitCounts >>= maskToBits
  logInfo $ "MSAA Samples: " ++ show highestCount
  return highestCount


createCommandPool :: VkDevice -> QueueFamilyDatas -> IO VkCommandPool
createCommandPool device QueueFamilyDatas {..} = do
  let graphicsQueueIndex = (_graphicsQueueIndex _queueFamilyIndices)
  logInfo $ "Create Command Pool: graphicsFamilyIndex(" ++ show graphicsQueueIndex ++ ")"
  let commandPoolCreateInfo = createVk @VkCommandPoolCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"queueFamilyIndex" graphicsQueueIndex
  commandPool <- alloca $ \commandPoolPtr -> do
    withPtr commandPoolCreateInfo $ \createInfoPtr -> do
        result <- vkCreateCommandPool device createInfoPtr VK_NULL commandPoolPtr
        validationVK result "vkCreateCommandPool failed!"
    peek commandPoolPtr
  return commandPool


destroyCommandPool :: VkDevice -> VkCommandPool -> IO ()
destroyCommandPool device commandPool = do
  logInfo $ "Destroy Command Pool: " ++ show commandPool
  vkDestroyCommandPool device commandPool VK_NULL


createCommandBuffers :: VkDevice
                     -> VkCommandPool
                     -> Word32
                     -> IO (Ptr VkCommandBuffer)
createCommandBuffers device commandPool commandBufferCount = do
    commandBuffersPtr <- mallocArray (fromIntegral commandBufferCount)::IO (Ptr VkCommandBuffer)
    let allocationInfo = createVk @VkCommandBufferAllocateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"commandPool" commandPool
            &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
            &* set @"commandBufferCount" (fromIntegral commandBufferCount)
    withPtr allocationInfo $ \allocationInfoPtr -> do
        result <- vkAllocateCommandBuffers device allocationInfoPtr commandBuffersPtr
        validationVK result "vkAllocateCommandBuffers failed!"
    logInfo $ "Create Command Buffer: "  ++ show commandBufferCount ++ " " ++ (show commandBuffersPtr)
    commandBuffers <- peekArray (fromIntegral commandBufferCount) commandBuffersPtr
    return commandBuffersPtr


destroyCommandBuffers :: VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()
destroyCommandBuffers device commandPool bufferCount commandBuffersPtr = do
  vkFreeCommandBuffers device commandPool bufferCount commandBuffersPtr
  free commandBuffersPtr


createSemaphores :: VkDevice -> IO [VkSemaphore]
createSemaphores device = do
  semaphores <- allocaArray Constants.maxFrameCount $ \semaphoresPtr -> do
    let semaphoreCreateInfo = createVk @VkSemaphoreCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS    
    forM_ [0..(Constants.maxFrameCount - 1)] $ \index -> do
      withPtr semaphoreCreateInfo $ \semaphoreCreateInfoPtr -> do
          result <- vkCreateSemaphore device semaphoreCreateInfoPtr VK_NULL (ptrAtIndex semaphoresPtr index)
          validationVK result "vkCreateSemaphore failed!"
    peekArray Constants.maxFrameCount semaphoresPtr    
  logInfo $ "Create Semaphore: " ++ show semaphores
  return semaphores

destroySemaphores :: VkDevice -> [VkSemaphore] -> IO ()
destroySemaphores device semaphores = do  
  forM_ semaphores $ \semaphore -> 
    vkDestroySemaphore device semaphore VK_NULL
  logInfo $ "Destroy Semaphore: " ++ show semaphores

createFrameFences :: VkDevice -> IO (Ptr VkFence)
createFrameFences device = do
  frameFencesPtr <- mallocArray Constants.maxFrameCount
  let fenceCreateInfo = createVk @VkFenceCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_FENCE_CREATE_SIGNALED_BIT
  forM_ [0..(Constants.maxFrameCount - 1)] $ \index -> do
    withPtr fenceCreateInfo $ \fenceCreateInfoPtr -> do
        result <- vkCreateFence device fenceCreateInfoPtr VK_NULL (ptrAtIndex frameFencesPtr index)
        validationVK result "vkCreateSemaphore failed!"
  fences <- peekArray Constants.maxFrameCount frameFencesPtr
  logInfo $ "Create VkFences: " ++ show fences
  return frameFencesPtr  

destroyFrameFences :: VkDevice -> Ptr VkFence -> IO ()
destroyFrameFences device frameFencesPtr = do
  fences <- peekArray Constants.maxFrameCount frameFencesPtr
  logInfo $ "Destroy VkFence: " ++ show fences
  forM_ fences $ \fence -> 
    vkDestroyFence device fence VK_NULL
  free frameFencesPtr  

drawFrame :: RendererData -> Int -> Ptr Word32 -> IO VkResult
drawFrame RendererData {..} frameIndex imageIndexPtr = do
  let SwapChainData {..} = _swapChainData
      QueueFamilyDatas {..} = _queueFamilyDatas
      frameFencePtr = ptrAtIndex _frameFencesPtr frameIndex
      imageAvailableSemaphore = _imageAvailableSemaphores !! frameIndex
      renderFinishedSemaphore = _renderFinishedSemaphores !! frameIndex
  
  vkWaitForFences _device 1 frameFencePtr VK_TRUE (maxBound :: Word64) >>=
    flip validationVK "vkWaitForFences failed!"
  
  --  validationVK result "vkAcquireNextImageKHR failed!"
  result <- vkAcquireNextImageKHR _device _swapChain maxBound imageAvailableSemaphore VK_NULL_HANDLE imageIndexPtr
  if (VK_SUCCESS /= result) then 
    return result
  else do
    imageIndex <- peek imageIndexPtr  
    let submitInfo = createVk @VkSubmitInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
          &* set @"pNext" VK_NULL
          &* set @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [imageAvailableSemaphore]
          &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          &* set @"commandBufferCount" 1
          &* set @"pCommandBuffers" (ptrAtIndex _commandBuffersPtr (fromIntegral imageIndex))
          &* set @"signalSemaphoreCount" 1
          &* setListRef @"pSignalSemaphores" [renderFinishedSemaphore]
    
    withPtr submitInfo $ \submitInfoPtr ->
        vkQueueSubmit _graphicsQueue 1 submitInfoPtr VK_NULL >>=
          flip validationVK "vkQueueSubmit failed!"

    let presentInfo = createVk @VkPresentInfoKHR
          $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
          &* set @"pNext" VK_NULL
          &* set @"pImageIndices" imageIndexPtr
          &* set @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [renderFinishedSemaphore]
          &* set @"swapchainCount" 1
          &* setListRef @"pSwapchains" [_swapChain]

    presentResult <- withPtr presentInfo $ \presentInfoPtr -> do
      vkQueuePresentKHR _presentQueue presentInfoPtr
    vkQueueWaitIdle _presentQueue >>= flip validationVK "vkQueueWaitIdle failed!"
    return presentResult


getDefaultRendererData :: IO RendererData
getDefaultRendererData = do
    imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
        writeField @"width" extentPtr $ 0
        writeField @"height" extentPtr $ 0
    surfaceCapabilities <- newVkData @VkSurfaceCapabilitiesKHR $ \surfaceCapabilitiesPtr -> do
        return ()
    let defaultSwapChainData = SwapChainData
            { _swapChain = VK_NULL
            , _swapChainImages = []
            , _swapChainImageCount = 0
            , _swapChainImageFormat = VK_FORMAT_UNDEFINED
            , _swapChainImageViews = []
            , _swapChainExtent = imageExtent }
        defaultSwapChainSupportDetails = SwapChainSupportDetails
            { _capabilities = surfaceCapabilities
            , _formats = []
            , _presentModes = [] }
        defaultQueueFamilyIndices = QueueFamilyIndices
            { _graphicsQueueIndex = 0
            , _presentQueueIndex = 0
            , _computeQueueIndex = 0
            , _transferQueueIndex = 0
            , _sparseBindingQueueIndex = 0 }
        defaultQueueFamilyDatas = QueueFamilyDatas
            { _graphicsQueue = VK_NULL
            , _presentQueue = VK_NULL
            , _queueFamilyIndexList = []
            , _queueFamilyCount = 0
            , _queueFamilyIndices = defaultQueueFamilyIndices }
    return RendererData
      { _msaaSamples = VK_SAMPLE_COUNT_4_BIT
      , _imageAvailableSemaphores = []
      , _renderFinishedSemaphores = []
      , _vkInstance = VK_NULL
      , _vkSurface = VK_NULL
      , _device = VK_NULL
      , _physicalDevice = VK_NULL
      , _swapChainData = defaultSwapChainData
      , _swapChainSupportDetails = defaultSwapChainSupportDetails
      , _queueFamilyDatas = defaultQueueFamilyDatas
      , _frameFencesPtr = VK_NULL
      , _commandPool = VK_NULL
      , _commandBufferCount = 0
      , _commandBuffersPtr = VK_NULL }


createGraphicsRenderPass :: RendererData -> [Float] -> IO RenderPassData
createGraphicsRenderPass rendererData@RendererData {..} clearValues = do
    let vertexShaderFile = "Resource/Shaders/triangle.vert"
        fragmentShaderFile = "Resource/Shaders/triangle.frag"
        imageFormat = _swapChainImageFormat _swapChainData
        imageExtent = _swapChainExtent _swapChainData
        imageViews = _swapChainImageViews _swapChainData

    renderPassData <- createRenderPassData _device vertexShaderFile fragmentShaderFile imageFormat imageExtent imageViews clearValues
    recordCommandBuffer rendererData renderPassData
    return renderPassData

destroyGraphicsRenderPass :: RendererData -> RenderPassData -> IO ()
destroyGraphicsRenderPass rendererData renderPassData = do
    destroyRenderPassData (_device rendererData) renderPassData


createRenderer :: RendererData
               -> GLFW.Window
               -> String
               -> String
               -> Bool
               -> [CString]
               -> IO RendererData
createRenderer defaultRendererData window progName engineName isConcurrentMode requireExtensions = do
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
      queueFamilyDatas =
        QueueFamilyDatas
          { _graphicsQueue = fromMaybe defaultQueue $ Map.lookup graphicsQueueIndex queueMap
          , _presentQueue = fromMaybe defaultQueue $ Map.lookup presentQueueIndex queueMap
          , _queueFamilyIndexList = queueFamilyIndexList
          , _queueFamilyCount = fromIntegral $ length queueMap
          , _queueFamilyIndices = queueFamilyIndices }
  commandPool <- createCommandPool device queueFamilyDatas
  imageAvailableSemaphores <- createSemaphores device
  renderFinishedSemaphores <- createSemaphores device
  frameFencesPtr <- createFrameFences device
  swapChainData <- createSwapChainData device swapChainSupportDetails queueFamilyDatas vkSurface
  let commandBufferCount = _swapChainImageCount swapChainData
  commandBuffersPtr <- createCommandBuffers device commandPool commandBufferCount
  let rendererData = defaultRendererData
        { _msaaSamples = msaaSamples
        , _imageAvailableSemaphores = imageAvailableSemaphores
        , _renderFinishedSemaphores = renderFinishedSemaphores
        , _vkInstance = vkInstance
        , _vkSurface = vkSurface
        , _device = device
        , _physicalDevice = physicalDevice
        , _queueFamilyDatas = queueFamilyDatas
        , _frameFencesPtr = frameFencesPtr
        , _commandPool = commandPool
        , _commandBuffersPtr = commandBuffersPtr
        , _commandBufferCount = commandBufferCount
        , _swapChainData = swapChainData
        , _swapChainSupportDetails = swapChainSupportDetails }
  return rendererData

destroyRenderer :: RendererData -> IO ()
destroyRenderer rendererData@RendererData {..} = do
    destroySemaphores _device _renderFinishedSemaphores
    destroySemaphores _device _imageAvailableSemaphores
    destroyFrameFences _device _frameFencesPtr
    destroyCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
    destroyCommandPool _device _commandPool
    destroySwapChainData _device _swapChainData
    destroyDevice _device
    destroyVkSurface _vkInstance _vkSurface
    destroyVulkanInstance _vkInstance

recreateSwapChain :: RendererData -> GLFW.Window -> IO RendererData
recreateSwapChain rendererData@RendererData {..} window = do
    destroyCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
    destroySwapChainData _device _swapChainData

    newSwapChainSupportDetails <- querySwapChainSupport _physicalDevice _vkSurface
    newSwapChainData <- createSwapChainData _device newSwapChainSupportDetails _queueFamilyDatas _vkSurface
    let commandBufferCount = _swapChainImageCount newSwapChainData
    newCommandBuffersPtr <- createCommandBuffers _device _commandPool commandBufferCount

    return rendererData
        { _swapChainSupportDetails = newSwapChainSupportDetails
        , _swapChainData = newSwapChainData
        , _commandBuffersPtr = newCommandBuffersPtr }


recordCommandBuffer :: RendererData -> RenderPassData -> IO ()
recordCommandBuffer rendererData renderPassData = do
    let frameBufferData = _frameBufferData renderPassData
        frameBuffers = _frameBuffers frameBufferData
        imageExtent = _frameBufferSize frameBufferData
        graphicsPipelineData = _graphicsPipelineData renderPassData
        graphicsPipeline = _pipeline graphicsPipelineData
        renderPass = _renderPass renderPassData
        clearValues = _frameBufferClearValues frameBufferData
        commandBufferCount = _commandBufferCount rendererData
        commandBuffersPtr = _commandBuffersPtr rendererData
    commandBuffers <- peekArray (fromIntegral commandBufferCount) commandBuffersPtr
    -- record command buffers
    forM_ (zip frameBuffers commandBuffers) $ \(frameBuffer, commandBuffer) -> do
        let commandBufferBeginInfo :: VkCommandBufferBeginInfo
            commandBufferBeginInfo = createVk @VkCommandBufferBeginInfo
                $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                &* set @"pNext" VK_NULL
                &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
            renderPassBeginInfo :: VkRenderPassBeginInfo
            renderPassBeginInfo = createVk @VkRenderPassBeginInfo
                $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
                &* set @"pNext" VK_NULL
                &* set @"renderPass" renderPass
                &* set @"framebuffer" frameBuffer
                &* setVk @"renderArea" (
                    setVk @"offset" ( set @"x" 0 &* set @"y" 0 )
                    &* set @"extent" imageExtent )
                &* set @"clearValueCount" 1
                &* setVkRef @"pClearValues"
                    ( createVk $ setVk @"color"
                        $  setAt @"float32" @0 (clearValues !! 0)
                        &* setAt @"float32" @1 (clearValues !! 1)
                        &* setAt @"float32" @2 (clearValues !! 2)
                        &* setAt @"float32" @3 (clearValues !! 3) )
        -- begin
        withPtr commandBufferBeginInfo $ \commandBufferBeginInfoPtr -> do
            result <- vkBeginCommandBuffer commandBuffer commandBufferBeginInfoPtr
            validationVK result "vkBeginCommandBuffer failed!"
        withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
            vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE
        vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        -- draw
        vkCmdDraw commandBuffer 3 1 0 0
        -- end
        vkCmdEndRenderPass commandBuffer
        vkEndCommandBuffer commandBuffer >>= flip validationVK "vkEndCommandBuffer failed!"


runCommandsOnce :: VkDevice
                -> VkCommandPool
                -> VkQueue
                -> (VkCommandBuffer -> IO ())
                -> IO ()
runCommandsOnce device commandPool commandQueue action = do
  let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandPool" commandPool
          &* set @"commandBufferCount" 1
          &* set @"pNext" VK_NULL
          
  allocaPeek $ \commandBufferPtr -> do
    withPtr allocInfo $ \allocInfoPtr -> do
      vkAllocateCommandBuffers device allocInfoPtr commandBufferPtr
    commandBuffer <- peek commandBufferPtr

    let beginInfo = createVk @VkCommandBufferBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
            &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
            &* set @"pNext" VK_NULL

    withPtr beginInfo $ \beginInfoPtr -> do
      vkBeginCommandBuffer commandBuffer beginInfoPtr

    -- run action
    action commandBuffer

    vkEndCommandBuffer commandBuffer

    let submitInfo = createVk @VkSubmitInfo
            $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
            &* set @"pNext" VK_NULL
            &* set @"waitSemaphoreCount" 0
            &* set @"pWaitSemaphores"   VK_NULL
            &* set @"pWaitDstStageMask" VK_NULL
            &* set @"commandBufferCount" 1
            &* set @"pCommandBuffers" commandBufferPtr
            &* set @"signalSemaphoreCount" 0
            &* set @"pSignalSemaphores" VK_NULL

    {- TODO: a real app would need a better logic for waiting.

             In the example below, we create a new fence every time we want to
             execute a single command. Then, we attach this fence to our command.
             vkWaitForFences makes the host (CPU) wait until the command is executed.
             The other way to do this thing is vkQueueWaitIdle.

             I guess, a good approach could be to pass the fence to this function
             from the call site. The call site would decide when it wants to wait
             for this command to finish.

             Even if we don't pass the fence from outside, maybe we should create
             the fence oustise of the innermost `locally` scope. This way, the
             fence would be shared between calls (on the other hand, a possible
             concurrency would be hurt in this case).
           -}
    -- locally $ do
    --   fence <- createFence dev False
    --   withVkPtr submitInfo $ \siPtr ->
    --     runVk $ vkQueueSubmit cmdQueue 1 siPtr fence
    --   fencePtr <- newArrayRes [fence]
    --   runVk $ vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)

    withPtr submitInfo $ \submitInfoPtr -> do
      vkQueueSubmit commandQueue 1 submitInfoPtr VK_NULL_HANDLE
    vkQueueWaitIdle commandQueue

    vkFreeCommandBuffers device commandPool 1 commandBufferPtr
  return ()
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
  ( QueueFamilyIndices (..)
  , QueueFamilyDatas (..)  
  , SwapChainSupportDetails (..)
  , SwapChainData (..)
  , GraphicsPipelineData (..)
  , RenderData (..)
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
  , getQueueFamilyIndices
  , createQueues
  , createDevice
  , destroyDevice
  , createSwapChainData
  , destroySwapChainData
  , createRenderPass
  , destroyRenderPass
  , createGraphicsPipeline
  , destroyGraphicsPipeline
  , createFramebuffers
  , destroyFramebuffers
  , createCommandPool
  , destroyCommandPool
  , createCommandBuffers
  , destroyCommandBuffers
  , createSemaphores
  , destroySemaphores
  , createFrameFences
  , destroyFrameFences
  , drawFrame
  , getDefaultRenderData
  , createRenderData
  , destroyRenderData
  , createRenderer
  , destroyRenderer  
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

import Library.Shader
import Library.Utils
import Library.Logger
import qualified Library.Constants as Constants


data QueueFamilyIndices = QueueFamilyIndices
  { _graphicsQueueIndex :: Word32
  , _presentQueueIndex :: Word32
  , _computeQueueIndex :: Word32
  , _transferQueueIndex :: Word32
  , _sparseBindingQueueIndex :: Word32
  } deriving (Eq, Show)

data QueueFamilyDatas = QueueFamilyDatas
  { _graphicsQueue :: VkQueue
  , _presentQueue :: VkQueue
  , _queueFamilyIndexList :: [Word32]
  , _queueFamilyCount :: Word32
  , _queueFamilyIndices :: QueueFamilyIndices  
  } deriving (Eq, Show)

data SwapChainSupportDetails = SwapChainSupportDetails
  { _capabilities :: VkSurfaceCapabilitiesKHR
  , _formats      :: [VkSurfaceFormatKHR]
  , _presentModes :: [VkPresentModeKHR]
  } deriving (Eq, Show)

data SwapChainData = SwapChainData
  { _swapChain :: VkSwapchainKHR
  , _swapChainImages :: [VkImage]
  , _swapChainImageFormat :: VkFormat
  , _swapChainImageViews :: [VkImageView]
  , _swapChainExtent :: VkExtent2D
  } deriving (Eq, Show)

data GraphicsPipelineData = GraphicsPipelineData
  { _vertexShaderCreateInfo :: VkPipelineShaderStageCreateInfo
  , _fragmentShaderCreateInfo :: VkPipelineShaderStageCreateInfo
  , _pipelineLayout :: VkPipelineLayout
  , _pipeline :: VkPipeline  
  } deriving (Eq, Show)

data RenderData = RenderData
  { _msaaSamples :: VkSampleCountBitmask FlagBit
  , _imageAvailableSemaphores :: [VkSemaphore]
  , _renderFinishedSemaphores :: [VkSemaphore]
  , _vkInstance :: VkInstance
  , _vkSurface :: VkSurfaceKHR
  , _device :: VkDevice
  , _physicalDevice :: VkPhysicalDevice
  , _swapChainData :: SwapChainData
  , _queueFamilyDatas :: QueueFamilyDatas
  , _frameFencesPtr :: Ptr VkFence
  , _frameBuffers :: [VkFramebuffer]
  , _commandPool :: VkCommandPool
  , _commandBufferCount :: Word32
  , _commandBuffersPtr :: Ptr VkCommandBuffer
  , _graphicsPipelineData :: GraphicsPipelineData
  , _renderPass :: VkRenderPass
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

selectQueueFamily :: VkQueueBitmask FlagMask -> [(Word32, VkQueueFamilyProperties)] -> IO [Word32]
selectQueueFamily _ [] = return []
selectQueueFamily requireQueueFlag ((queueFamilyIndex, queueFamilyProperty):xs) = do
  if 0 < queueCount && (queueFlags .&. requireQueueFlag) /= zeroBits
    then do
      result <- selectQueueFamily requireQueueFlag xs
      return (queueFamilyIndex:result)
    else
      selectQueueFamily requireQueueFlag xs
  where
    queueCount = getField @"queueCount" queueFamilyProperty
    queueFlags = getField @"queueFlags" queueFamilyProperty

selectPresentationFamily :: VkPhysicalDevice
                         -> VkSurfaceKHR
                         -> [(Word32, VkQueueFamilyProperties)]
                         -> IO [Word32]
selectPresentationFamily _ _ [] = return []
selectPresentationFamily device surface (queueFamilies:xs) = do
  let queueFamilyIndex = fst queueFamilies
  supported <- alloca $ \supportedPtr -> do
    result <- vkGetPhysicalDeviceSurfaceSupportKHR device queueFamilyIndex surface supportedPtr
    validationVK result "vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support."
    peek supportedPtr
  if VK_TRUE == supported
    then do
      result <- selectPresentationFamily device surface xs
      return (queueFamilyIndex:result)
    else
      selectPresentationFamily device surface xs

getQueueFamilies :: VkPhysicalDevice -> IO [(Word32, VkQueueFamilyProperties)]
getQueueFamilies physicalDevice = alloca $ \queueFamilyCountPtr -> do
  vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueFamilyCountPtr VK_NULL_HANDLE
  familyCount <- fromIntegral <$> peek queueFamilyCountPtr
  when (familyCount <= 0) $ throwVKMsg "Zero queue family count!"
  logInfo $ "Found " ++ show familyCount ++ " queue families."
  queueFaimilies <- allocaArray familyCount $ \familiesPtr -> do
    vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueFamilyCountPtr familiesPtr
    zip [0..] <$> peekArray familyCount familiesPtr
  mapM_ (\(x,y) -> logInfo $ "    [" ++ (show x) ++ "] " ++ (show y) ) queueFaimilies
  return queueFaimilies

getQueueFamilyIndices :: VkPhysicalDevice -> VkSurfaceKHR -> Bool -> IO QueueFamilyIndices
getQueueFamilyIndices physicalDevice vkSurface isConcurrentMode = do 
  queueFaimilies <- getQueueFamilies physicalDevice  
  presentationFamilyIndices <- selectPresentationFamily physicalDevice vkSurface queueFaimilies  
  graphicsQueueIndices <- selectQueueFamily VK_QUEUE_GRAPHICS_BIT queueFaimilies
  computeFamilyIndices <- selectQueueFamily VK_QUEUE_COMPUTE_BIT queueFaimilies
  transferFamilyIndices <- selectQueueFamily VK_QUEUE_TRANSFER_BIT queueFaimilies
  sparseBindingFamilyIndices <- selectQueueFamily VK_QUEUE_SPARSE_BINDING_BIT queueFaimilies  
  let
    defaultIndex = graphicsQueueIndices !! 0
    queueFamilyIndices = QueueFamilyIndices
      { _graphicsQueueIndex = defaultIndex
      , _presentQueueIndex = getFamilyIndex presentationFamilyIndices defaultIndex
      , _computeQueueIndex = getFamilyIndex computeFamilyIndices defaultIndex
      , _transferQueueIndex = getFamilyIndex transferFamilyIndices defaultIndex
      , _sparseBindingQueueIndex = getFamilyIndex sparseBindingFamilyIndices defaultIndex
      }    
  logInfo $ "Graphics Queue Index : " ++ show (_graphicsQueueIndex queueFamilyIndices)
  logInfo $ "Presentation Queue Index : " ++ show (_presentQueueIndex queueFamilyIndices) ++ " / " ++ show presentationFamilyIndices
  logInfo $ "Computer Queue Index : " ++ show (_computeQueueIndex queueFamilyIndices) ++ " / " ++ show computeFamilyIndices
  logInfo $ "Transfer Queue Index : " ++ show (_transferQueueIndex queueFamilyIndices) ++ " / " ++ show transferFamilyIndices
  logInfo $ "Sparse Binding Queue Index : " ++ show (_sparseBindingQueueIndex queueFamilyIndices)  ++ " / " ++ show sparseBindingFamilyIndices
  return queueFamilyIndices
  where
    getFamilyIndex [] _ = Constants.invalidQueueIndex
    getFamilyIndex indices defaultIndex = 
      let result = [x | x <- indices, x /= defaultIndex]
      in
        if isConcurrentMode && (elem defaultIndex indices) then defaultIndex
        else if 0 < (length result) then result !! 0
        else defaultIndex

createQueues :: VkDevice -> [Word32] -> IO (Map.Map Word32 VkQueue)
createQueues device queueFamilyIndices = do
  queueList <- forM queueFamilyIndices $ \queueFamilyIndex -> do
    queue <- alloca $ \queuePtr -> do
      vkGetDeviceQueue device queueFamilyIndex 0 queuePtr
      peek queuePtr
    return (queueFamilyIndex, queue)
  logInfo $ "Created Queues: " ++ show queueList
  return $ Map.fromList queueList

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

chooseSwapSurfaceFormat :: SwapChainSupportDetails -> IO VkSurfaceFormatKHR
chooseSwapSurfaceFormat swapChainSupportDetails =
  if 1 == length formats && VK_FORMAT_UNDEFINED == getFormat (head formats) then
    newVkData $ \surfaceFormatPtr -> do
      writeField @"format" surfaceFormatPtr VK_FORMAT_B8G8R8A8_UNORM
      writeField @"colorSpace" surfaceFormatPtr VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  else
    findAvailableFormat formats
  where
    formats = _formats swapChainSupportDetails
    getFormat = getField @"format"
    getColorSpace = getField @"colorSpace"    
    findAvailableFormat :: [VkSurfaceFormatKHR] -> IO VkSurfaceFormatKHR
    findAvailableFormat [] = return $ head formats
    findAvailableFormat (x:xs) =
      if VK_FORMAT_B8G8R8A8_UNORM == getFormat x && VK_COLOR_SPACE_SRGB_NONLINEAR_KHR == getColorSpace x
      then return x
      else findAvailableFormat xs

chooseSwapPresentMode :: SwapChainSupportDetails -> IO VkPresentModeKHR
chooseSwapPresentMode swapChainSupportDetails
  | VK_PRESENT_MODE_FIFO_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_KHR
  | VK_PRESENT_MODE_MAILBOX_KHR `elem` presentModes = return VK_PRESENT_MODE_MAILBOX_KHR
  | VK_PRESENT_MODE_FIFO_RELAXED_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_RELAXED_KHR
  | VK_PRESENT_MODE_IMMEDIATE_KHR `elem` presentModes = return VK_PRESENT_MODE_IMMEDIATE_KHR
  | otherwise = return VK_PRESENT_MODE_FIFO_KHR
  where
    presentModes = _presentModes swapChainSupportDetails

chooseSwapExtent :: SwapChainSupportDetails -> IO VkExtent2D
chooseSwapExtent swapChainSupportDetails = do
  imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
    writeField @"width" extentPtr $ max (width $ getField @"minImageExtent" capabilities)
                             $ min (width $ getField @"maxImageExtent" capabilities)
                                   (width $ getField @"currentExtent"  capabilities)
    writeField @"height" extentPtr $ max (height $ getField @"minImageExtent" capabilities)
                              $ min (height $ getField @"maxImageExtent" capabilities)
                                    (height $ getField @"currentExtent"  capabilities)
  return imageExtent
  where
    capabilities = _capabilities swapChainSupportDetails
    width = getField @"width"
    height = getField @"height"

createSwapChainData :: VkDevice 
                    -> SwapChainSupportDetails
                    -> QueueFamilyDatas 
                    -> VkSurfaceKHR 
                    -> IO SwapChainData
createSwapChainData device swapChainSupportDetails queueFamilyDatas vkSurface = do
  surfaceFormat <- chooseSwapSurfaceFormat swapChainSupportDetails
  presentMode <- chooseSwapPresentMode swapChainSupportDetails
  imageExtent <- chooseSwapExtent swapChainSupportDetails
  queueFamilyIndicesPtr <- newArray (_queueFamilyIndexList queueFamilyDatas)

  -- try tripple buffering
  let maxImageCount = getField @"maxImageCount" $ _capabilities swapChainSupportDetails
      minImageCount = getField @"minImageCount" $ _capabilities swapChainSupportDetails
      imageCount' = if maxImageCount <= 0
                   then max minImageCount Constants.imageCount
                   else min maxImageCount $ max minImageCount Constants.imageCount

  -- write VkSwapchainCreateInfoKHR
  swapChainCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swapChainCreateInfoPtr -> do
    writeField @"sType" swapChainCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
    writeField @"pNext" swapChainCreateInfoPtr VK_NULL_HANDLE
    writeField @"flags" swapChainCreateInfoPtr VK_ZERO_FLAGS
    writeField @"surface" swapChainCreateInfoPtr vkSurface
    writeField @"minImageCount" swapChainCreateInfoPtr imageCount'
    writeField @"imageFormat" swapChainCreateInfoPtr (getField @"format" surfaceFormat)
    writeField @"imageColorSpace" swapChainCreateInfoPtr (getField @"colorSpace" surfaceFormat)
    writeField @"imageExtent" swapChainCreateInfoPtr imageExtent
    writeField @"imageArrayLayers" swapChainCreateInfoPtr 1
    writeField @"imageUsage" swapChainCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    if (_graphicsQueue queueFamilyDatas) /= (_presentQueue queueFamilyDatas)
    then do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_CONCURRENT
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr (_queueFamilyCount queueFamilyDatas)
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr queueFamilyIndicesPtr
    else do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr 0
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr VK_NULL_HANDLE      
    writeField @"preTransform" swapChainCreateInfoPtr (getField @"currentTransform" $ _capabilities swapChainSupportDetails)
    writeField @"compositeAlpha" swapChainCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
    writeField @"presentMode" swapChainCreateInfoPtr presentMode
    writeField @"clipped" swapChainCreateInfoPtr VK_TRUE
    writeField @"oldSwapchain" swapChainCreateInfoPtr VK_NULL_HANDLE
  logInfo "Create SwapChain"
  logInfo $ "    imageCount : " ++ (show imageCount')
  logInfo $ "    imageFormat : " ++ (show $ getField @"imageFormat" swapChainCreateInfo)
  logInfo $ "    imageColorSpace : " ++ (show $ getField @"imageColorSpace" swapChainCreateInfo)
  logInfo $ "    imageExtent : " ++ (show $ getField @"imageExtent" swapChainCreateInfo)
  logInfo $ "    imageSharingMode : " ++ (show $ getField @"imageSharingMode" swapChainCreateInfo)

  swapChain <- alloca $ \swapChainPtr -> do
      result <- vkCreateSwapchainKHR device (unsafePtr swapChainCreateInfo) VK_NULL_HANDLE swapChainPtr
      validationVK result "vkCreateSwapchainKHR failed!"
      peek swapChainPtr

  swapChainImages <- asListVK $ \counterPtr valueArrayPtr -> do
      result <- vkGetSwapchainImagesKHR device swapChain counterPtr valueArrayPtr
      validationVK result "vkGetSwapchainImagesKHR error"

  let swapChainImageFormat = getField @"imageFormat" swapChainCreateInfo
      swapChainExtent = (getField @"imageExtent" swapChainCreateInfo)
  
  touchVkData swapChainCreateInfo
  free queueFamilyIndicesPtr

  swapChainImageViews <- createSwapChainImageViews device swapChainImages swapChainImageFormat
  let swapChainData = SwapChainData { _swapChain = swapChain
                                    , _swapChainImages = swapChainImages
                                    , _swapChainImageFormat = swapChainImageFormat
                                    , _swapChainImageViews = swapChainImageViews
                                    , _swapChainExtent = swapChainExtent
                                    }
  return swapChainData

destroySwapChainData :: VkDevice -> SwapChainData -> IO ()
destroySwapChainData device swapChainData = do
  destroySwapChainImageViews device (_swapChainImageViews swapChainData)
  logInfo "Destroy SwapChain"
  vkDestroySwapchainKHR device (_swapChain swapChainData) VK_NULL_HANDLE

createSwapChainImageViews :: VkDevice -> [VkImage] -> VkFormat -> IO [VkImageView]
createSwapChainImageViews device swapChainImages swapChainImageFormat = do
  components <- (newVkData $ \componentsPtr -> do
    writeField @"r" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
    writeField @"g" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
    writeField @"b" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
    writeField @"a" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY)::IO VkComponentMapping
  subresourceRange <- (newVkData $ \subresourceRangePtr -> do
    writeField @"aspectMask" subresourceRangePtr VK_IMAGE_ASPECT_COLOR_BIT
    writeField @"baseMipLevel" subresourceRangePtr 0
    writeField @"levelCount" subresourceRangePtr 1
    writeField @"baseArrayLayer" subresourceRangePtr 0
    writeField @"layerCount" subresourceRangePtr 1)::IO VkImageSubresourceRange
  let 
    getImageViewCreateInfo :: VkImage -> IO VkImageViewCreateInfo
    getImageViewCreateInfo image = newVkData @VkImageViewCreateInfo $ \viewPtr -> do
      writeField @"sType" viewPtr VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      writeField @"pNext" viewPtr VK_NULL_HANDLE
      writeField @"flags" viewPtr VK_ZERO_FLAGS
      writeField @"image" viewPtr image
      writeField @"viewType" viewPtr VK_IMAGE_VIEW_TYPE_2D
      writeField @"format" viewPtr swapChainImageFormat
      writeField @"components" viewPtr components
      writeField @"subresourceRange" viewPtr subresourceRange  
  imageViewCreateInfos <- mapM getImageViewCreateInfo swapChainImages
  imageViews <- forM imageViewCreateInfos $ \imageViewCreateInfo ->
    alloca $ \imageViewPtr -> do
        result <- vkCreateImageView device (unsafePtr imageViewCreateInfo) VK_NULL_HANDLE imageViewPtr
        validationVK result "vkCreateImageView error"
        peek imageViewPtr
  mapM_ touchVkData imageViewCreateInfos
  return imageViews

destroySwapChainImageViews :: VkDevice -> [VkImageView] -> IO ()
destroySwapChainImageViews device imageViews = do
  mapM_ (\imageView -> vkDestroyImageView device imageView VK_NULL_HANDLE) imageViews
  

createRenderPass :: VkDevice -> SwapChainData -> IO VkRenderPass
createRenderPass device swapChainData =
  let
    colorAttachment :: VkAttachmentDescription
    colorAttachment = createVk @VkAttachmentDescription
      $  set @"flags" VK_ZERO_FLAGS
      &* set @"format" (_swapChainImageFormat swapChainData)
      &* set @"samples" VK_SAMPLE_COUNT_1_BIT
      &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
      &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
      &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
      &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
      &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
      &* set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

    colorAttachmentRef :: VkAttachmentReference
    colorAttachmentRef = createVk @VkAttachmentReference
      $  set @"attachment" 0
      &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

    subpass :: VkSubpassDescription
    subpass = createVk @VkSubpassDescription
      $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
      &* set @"colorAttachmentCount" 1
      &* setVkRef @"pColorAttachments" colorAttachmentRef
      &* set @"pPreserveAttachments" VK_NULL
      &* set @"pInputAttachments" VK_NULL

    dependency :: VkSubpassDependency
    dependency = createVk @VkSubpassDependency
      $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
      &* set @"dstSubpass" 0
      &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* set @"srcAccessMask" VK_ZERO_FLAGS
      &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* set @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)

    renderPassCreateInfo :: VkRenderPassCreateInfo
    renderPassCreateInfo = createVk @VkRenderPassCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"attachmentCount" 1
      &* setVkRef @"pAttachments" colorAttachment
      &* set @"subpassCount" 1
      &* setVkRef @"pSubpasses" subpass
      &* set @"dependencyCount" 1
      &* setVkRef @"pDependencies" dependency
  in do
    renderPass <- alloca $ \renderPassPtr -> do
        result <- vkCreateRenderPass device (unsafePtr renderPassCreateInfo) VK_NULL renderPassPtr
        validationVK result "vkCreatePipelineLayout failed!"
        peek renderPassPtr
    touchVkData renderPassCreateInfo
    logInfo $ "Create RenderPass: " ++ show (_swapChainImageFormat swapChainData)
    return renderPass

destroyRenderPass :: VkDevice -> VkRenderPass -> IO ()
destroyRenderPass device renderPass = do
  logInfo "Destroy RenderPass"
  vkDestroyRenderPass device renderPass VK_NULL

createPipelineLayout :: VkDevice -> IO VkPipelineLayout
createPipelineLayout device = do  
  let 
    pipelineCreateInfo :: VkPipelineLayoutCreateInfo
    pipelineCreateInfo = createVk @VkPipelineLayoutCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"setLayoutCount" 0      
      &* set @"pSetLayouts" VK_NULL
      &* set @"pushConstantRangeCount" 0      
      &* set @"pPushConstantRanges" VK_NULL

  pipelineLayout <- alloca $ \pipelineLayoutPtr -> do
      result <- vkCreatePipelineLayout device (unsafePtr pipelineCreateInfo) VK_NULL pipelineLayoutPtr
      validationVK result "vkCreatePipelineLayout failed!"
      logInfo "Create PipelineLayout"
      peek pipelineLayoutPtr

  touchVkData pipelineCreateInfo
  return pipelineLayout

destroyPipelineLayout :: VkDevice -> VkPipelineLayout -> IO ()
destroyPipelineLayout device pipelineLayout = do
  logInfo "Destroy PipelineLayout"
  vkDestroyPipelineLayout device pipelineLayout VK_NULL


createGraphicsPipeline :: VkDevice
                       -> VkExtent2D
                       -> String
                       -> String
                       -> VkRenderPass
                       -> IO GraphicsPipelineData
createGraphicsPipeline device swapChainExtent vertexShaderFile fragmentShaderFile renderPass = do
  vertexShaderCreateInfo <- createShaderStageCreateInfo device vertexShaderFile VK_SHADER_STAGE_VERTEX_BIT
  fragmentShaderCreateInfo <- createShaderStageCreateInfo device fragmentShaderFile VK_SHADER_STAGE_FRAGMENT_BIT
  let shaderStageInfos = [vertexShaderCreateInfo, fragmentShaderCreateInfo]
  pipelineLayout <- createPipelineLayout device
  let
    vertexInputInfo :: VkPipelineVertexInputStateCreateInfo
    vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"vertexBindingDescriptionCount" 0
      &* set @"pVertexBindingDescriptions" VK_NULL
      &* set @"vertexAttributeDescriptionCount" 0
      &* set @"pVertexAttributeDescriptions" VK_NULL

    inputAssembly :: VkPipelineInputAssemblyStateCreateInfo
    inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
      &* set @"primitiveRestartEnable" VK_FALSE

    viewPort :: VkViewport
    viewPort = createVk @VkViewport
      $  set @"x" 0
      &* set @"y" 0
      &* set @"width" (fromIntegral $ getField @"width" swapChainExtent)
      &* set @"height" (fromIntegral $ getField @"height" swapChainExtent)
      &* set @"minDepth" 0
      &* set @"maxDepth" 1

    scissorRect :: VkRect2D
    scissorRect = createVk @VkRect2D
      $  set   @"extent" swapChainExtent
      &* setVk @"offset" ( set @"x" 0 &* set @"y" 0 )

    viewPortState :: VkPipelineViewportStateCreateInfo
    viewPortState = createVk @VkPipelineViewportStateCreateInfo
      $ set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"viewportCount" 1
      &* setVkRef @"pViewports" viewPort
      &* set @"scissorCount" 1
      &* setVkRef @"pScissors" scissorRect

    rasterizer :: VkPipelineRasterizationStateCreateInfo
    rasterizer = createVk @VkPipelineRasterizationStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"depthClampEnable" VK_FALSE
      &* set @"rasterizerDiscardEnable" VK_FALSE
      &* set @"polygonMode" VK_POLYGON_MODE_FILL
      &* set @"cullMode" VK_CULL_MODE_BACK_BIT
      &* set @"frontFace" VK_FRONT_FACE_CLOCKWISE
      &* set @"depthBiasEnable" VK_FALSE
      &* set @"depthBiasConstantFactor" 0
      &* set @"depthBiasClamp" 0
      &* set @"depthBiasSlopeFactor" 0
      &* set @"lineWidth" 1.0

    multisampling :: VkPipelineMultisampleStateCreateInfo
    multisampling = createVk @VkPipelineMultisampleStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"sampleShadingEnable" VK_FALSE
      &* set @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT
      &* set @"minSampleShading" 1.0
      &* set @"pSampleMask" VK_NULL
      &* set @"alphaToCoverageEnable" VK_FALSE
      &* set @"alphaToOneEnable" VK_FALSE

    colorBlendAttachment :: VkPipelineColorBlendAttachmentState
    colorBlendAttachment = createVk @VkPipelineColorBlendAttachmentState
      $  set @"colorWriteMask" (VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT)
      &* set @"blendEnable" VK_FALSE
      &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE
      &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO
      &* set @"colorBlendOp" VK_BLEND_OP_ADD
      &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
      &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
      &* set @"alphaBlendOp" VK_BLEND_OP_ADD

    colorBlending :: VkPipelineColorBlendStateCreateInfo
    colorBlending = createVk @VkPipelineColorBlendStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"logicOpEnable" VK_FALSE
      &* set @"logicOp" VK_LOGIC_OP_COPY
      &* set @"attachmentCount" 1
      &* setVkRef @"pAttachments" colorBlendAttachment
      &* setAt @"blendConstants" @0 0.0
      &* setAt @"blendConstants" @1 0.0
      &* setAt @"blendConstants" @2 0.0
      &* setAt @"blendConstants" @3 0.0

    getGraphicsPipelineCreateInfo :: Ptr VkPipelineShaderStageCreateInfo -> Word32 -> VkGraphicsPipelineCreateInfo
    getGraphicsPipelineCreateInfo shaderStageInfosPtr shaderStageInfoCount = createVk @VkGraphicsPipelineCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"stageCount" shaderStageInfoCount
      &* set @"pStages" shaderStageInfosPtr
      &* setVkRef @"pVertexInputState" vertexInputInfo
      &* setVkRef @"pInputAssemblyState" inputAssembly
      &* set @"pTessellationState" VK_NULL
      &* setVkRef @"pViewportState" viewPortState
      &* setVkRef @"pRasterizationState" rasterizer
      &* setVkRef @"pMultisampleState" multisampling
      &* set @"pDepthStencilState" VK_NULL
      &* setVkRef @"pColorBlendState" colorBlending
      &* set @"pDynamicState" VK_NULL
      &* set @"layout" pipelineLayout
      &* set @"renderPass" renderPass
      &* set @"subpass" 0
      &* set @"basePipelineHandle" VK_NULL_HANDLE
      &* set @"basePipelineIndex" (-1)

  logInfo $ "Create Pipeline: " ++ show swapChainExtent  
  shaderStageInfosPtr <- newArray shaderStageInfos
  let graphicsPipelineCreateInfo = getGraphicsPipelineCreateInfo shaderStageInfosPtr (fromIntegral $ length shaderStageInfos)  
  createGraphicsPipelinesFunc <- vkGetDeviceProc @VkCreateGraphicsPipelines device  

  graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
      result <- createGraphicsPipelinesFunc device VK_NULL_HANDLE 1 (unsafePtr graphicsPipelineCreateInfo) VK_NULL graphicsPipelinePtr
      validationVK result "vkCreatePipelines failed!"
      peek graphicsPipelinePtr

  touchVkData graphicsPipelineCreateInfo
  free shaderStageInfosPtr

  return GraphicsPipelineData
    { _vertexShaderCreateInfo = vertexShaderCreateInfo
    , _fragmentShaderCreateInfo = fragmentShaderCreateInfo
    , _pipelineLayout = pipelineLayout
    , _pipeline = graphicsPipeline }

destroyGraphicsPipeline :: VkDevice -> GraphicsPipelineData -> IO ()
destroyGraphicsPipeline device graphicsPipelineData = do
  logInfo $ "Destroy GraphicsPipeline"
  let GraphicsPipelineData {..} = graphicsPipelineData
  vkDestroyPipeline device _pipeline VK_NULL
  destroyPipelineLayout device _pipelineLayout
  destroyShaderStageCreateInfo device _vertexShaderCreateInfo
  destroyShaderStageCreateInfo device _fragmentShaderCreateInfo

createFramebuffers :: VkDevice -> VkRenderPass -> SwapChainData -> IO [VkFramebuffer]
createFramebuffers device renderPass swapChainData = do  
  let swapChainImageViews' = (_swapChainImageViews swapChainData)  
  framebuffers <- mapM createFrameBuffer swapChainImageViews'
  logInfo $ "Create Framebuffers: " ++ show framebuffers
  return framebuffers
  where
    createFrameBuffer :: VkImageView -> IO VkFramebuffer
    createFrameBuffer swapChainImageView =
      let frameBufferCreateInfo = createVk @VkFramebufferCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"renderPass" renderPass
            &* set @"attachmentCount" 1
            &* setListRef @"pAttachments" [swapChainImageView]
            &* set @"width" (getField @"width" (_swapChainExtent swapChainData))
            &* set @"height" (getField @"height" (_swapChainExtent swapChainData))
            &* set @"layers" 1
      in do
        frameBuffer <- alloca $ \framebufferPtr -> do
            result <- vkCreateFramebuffer device (unsafePtr frameBufferCreateInfo) VK_NULL framebufferPtr
            validationVK result "vkCreateFramebuffer failed!"
            peek framebufferPtr
        touchVkData frameBufferCreateInfo
        return frameBuffer

destroyFramebuffers :: VkDevice -> [VkFramebuffer] -> IO ()
destroyFramebuffers device frameBuffers = do
  logInfo $ "Destroy Framebuffers" ++ show frameBuffers
  forM_ frameBuffers $ \frameBuffer ->
    vkDestroyFramebuffer device frameBuffer VK_NULL_HANDLE

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
                     -> VkPipeline
                     -> VkCommandPool
                     -> VkRenderPass
                     -> SwapChainData
                     -> [VkFramebuffer]
                     -> IO (Ptr VkCommandBuffer, Int)
createCommandBuffers device graphicsPipeline commandPool renderPass SwapChainData{..} frameBuffers = do
  let bufferCount = length frameBuffers
  commandBuffersPtr <- mallocArray bufferCount::IO (Ptr VkCommandBuffer)
  let allocationInfo = createVk @VkCommandBufferAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"commandPool" commandPool
        &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
        &* set @"commandBufferCount" (fromIntegral bufferCount)
  withPtr allocationInfo $ \allocationInfoPtr -> do
      result <- vkAllocateCommandBuffers device allocationInfoPtr commandBuffersPtr
      validationVK result "vkAllocateCommandBuffers failed!"
  logInfo $ "Create Command Buffer: "  ++ show bufferCount

  commandBuffers <- peekArray bufferCount commandBuffersPtr

  -- record command buffers  
  forM_ (zip frameBuffers commandBuffers) $ \(frameBuffer, commandBuffer) -> do    
    let
      commandBufferBeginInfo :: VkCommandBufferBeginInfo
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
            &* set @"extent" _swapChainExtent )
          &* set @"clearValueCount" 1
          &* setVkRef @"pClearValues"
              ( createVk $ setVk @"color"
                $  setAt @"float32" @0 0
                &* setAt @"float32" @1 0
                &* setAt @"float32" @2 0.2
                &* setAt @"float32" @3 1 )
    -- begin commands
    logInfo $ "    vkBeginCommandBuffer: " ++ show commandBuffer
    withPtr commandBufferBeginInfo $ \commandBufferBeginInfoPtr -> do
        result <- vkBeginCommandBuffer commandBuffer commandBufferBeginInfoPtr
        validationVK result "vkBeginCommandBuffer failed!"
    withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
      vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE
    -- basic drawing commands
    vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
    vkCmdDraw commandBuffer 3 1 0 0
    -- finishing up
    vkCmdEndRenderPass commandBuffer
    vkEndCommandBuffer commandBuffer >>= flip validationVK "vkEndCommandBuffer failed!"
  return (commandBuffersPtr, bufferCount)

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

drawFrame :: RenderData -> Int -> Ptr Word32 -> IO VkResult
drawFrame RenderData {..} frameIndex imageIndexPtr = do
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


getDefaultRenderData :: IO RenderData
getDefaultRenderData = do
  imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
    writeField @"width" extentPtr $ 0
    writeField @"height" extentPtr $ 0
  let 
    defaultSwapChainData = SwapChainData
      { _swapChain = VK_NULL
      , _swapChainImages = []
      , _swapChainImageFormat = VK_FORMAT_UNDEFINED
      , _swapChainImageViews = []
      , _swapChainExtent = imageExtent }
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
    deafaultShaderCreateInfo = createVk @VkPipelineShaderStageCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"stage" VK_SHADER_STAGE_VERTEX_BIT
      &* set @"module" VK_NULL
      &* setStrRef @"pName" "main"
    defaultGraphicsPipelineData = GraphicsPipelineData
      { _vertexShaderCreateInfo = deafaultShaderCreateInfo
      , _fragmentShaderCreateInfo = deafaultShaderCreateInfo
      , _pipelineLayout = VK_NULL
      , _pipeline = VK_NULL }
  return RenderData
      { _msaaSamples = VK_SAMPLE_COUNT_4_BIT
      , _imageAvailableSemaphores = []
      , _renderFinishedSemaphores = []
      , _vkInstance = VK_NULL
      , _vkSurface = VK_NULL
      , _device = VK_NULL
      , _physicalDevice = VK_NULL
      , _swapChainData = defaultSwapChainData
      , _queueFamilyDatas = defaultQueueFamilyDatas
      , _frameFencesPtr = VK_NULL
      , _frameBuffers = []
      , _commandPool = VK_NULL
      , _commandBufferCount = 0
      , _commandBuffersPtr = VK_NULL
      , _graphicsPipelineData = defaultGraphicsPipelineData
      , _renderPass = VK_NULL }

createRenderData :: RenderData -> SwapChainSupportDetails -> IO RenderData
createRenderData renderData@RenderData {..} swapChainSupportDetails = do
  swapChainData <- createSwapChainData _device swapChainSupportDetails _queueFamilyDatas _vkSurface
  renderPass <- createRenderPass _device swapChainData  
  let vertexShaderFile = "Resource/Shaders/triangle.vert"
      fragmentShaderFile = "Resource/Shaders/triangle.frag"
  graphicsPipelineData <- createGraphicsPipeline _device (_swapChainExtent swapChainData) vertexShaderFile fragmentShaderFile renderPass
  frameBuffers <- createFramebuffers _device renderPass swapChainData
  (commandBuffersPtr, commandBufferCount) <- createCommandBuffers _device (_pipeline graphicsPipelineData) _commandPool renderPass swapChainData frameBuffers
  return renderData 
    { _swapChainData = swapChainData
    , _renderPass = renderPass
    , _graphicsPipelineData = graphicsPipelineData
    , _frameBuffers = frameBuffers
    , _commandBuffersPtr = commandBuffersPtr
    , _commandBufferCount = fromIntegral commandBufferCount }
  
destroyRenderData :: RenderData -> IO ()
destroyRenderData RenderData {..} = do    
  destroyFramebuffers _device _frameBuffers
  destroyCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr  
  destroyGraphicsPipeline _device _graphicsPipelineData
  destroyRenderPass _device _renderPass  
  destroySwapChainData _device _swapChainData

createRenderer :: RenderData
               -> GLFW.Window
               -> String
               -> String
               -> Bool
               -> [CString]
               -> IO (RenderData, SwapChainSupportDetails)
createRenderer defaultRenderData window progName engineName isConcurrentMode requireExtensions = do
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
  let renderData = defaultRenderData
        { _msaaSamples = msaaSamples
        , _imageAvailableSemaphores = imageAvailableSemaphores
        , _renderFinishedSemaphores = renderFinishedSemaphores
        , _vkInstance = vkInstance
        , _vkSurface = vkSurface
        , _device = device
        , _physicalDevice = physicalDevice
        , _queueFamilyDatas = queueFamilyDatas
        , _frameFencesPtr = frameFencesPtr
        , _commandPool = commandPool }
  return (renderData, swapChainSupportDetails)

destroyRenderer :: RenderData -> IO ()
destroyRenderer renderData@RenderData {..} = do  
  destroyRenderData renderData

  destroySemaphores _device _renderFinishedSemaphores
  destroySemaphores _device _imageAvailableSemaphores 
  destroyFrameFences _device _frameFencesPtr  
  destroyCommandPool _device _commandPool
  destroyDevice _device
  destroyVkSurface _vkInstance _vkSurface
  destroyVulkanInstance _vkInstance


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
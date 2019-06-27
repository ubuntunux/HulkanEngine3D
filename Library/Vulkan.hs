{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
  ( vulkanLayers
  , requireDeviceExtensions
  , QueueFamilyIndices (..)
  , QueueFamilyDatas (..)  
  , SwapChainSupportDetails (..)
  , SwapChainData (..)
  , RenderData (..)
  , getInstanceExtensionSupport
  , getDeviceExtensionSupport
  , checkExtensionSupport
  , createVulkanInstance
  , destroyVulkanInstance
  , createVkSurface
  , destroyVkSurface
  , selectPhysicalDevice    
  , getPhysicalDeviceFeatures
  , getQueueFamilyIndices
  , getQueuePrioritiesPtr
  , getQueueCreateInfos
  , getDeviceCreateInfo
  , createQueues
  , createDevice
  , destroyDevice
  , createSwapChain
  , destroySwapChain
  , createSwapChainImageViews
  , destroySwapChainImageViews
  , createPipelineLayout
  , destroyPipelineLayout
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
  , createSemaphore
  , destroySemaphore
  , drawFrame
  ) where

import Control.Monad
import Data.Bits
import Data.List ((\\))
import qualified Data.Map as Map
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import qualified Graphics.UI.GLFW as GLFW
import Library.Utils


vulkanLayers :: [String]
vulkanLayers = ["VK_LAYER_LUNARG_standard_validation"]

requireDeviceExtensions :: [CString]
requireDeviceExtensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

invalidQueueIndex :: Word32
invalidQueueIndex = maxBound

data QueueFamilyIndices = QueueFamilyIndices
  { graphicsQueueIndex :: Word32
  , presentQueueIndex :: Word32
  , computeQueueIndex :: Word32
  , transferQueueIndex :: Word32
  , sparseBindingQueueIndex :: Word32
  } deriving (Eq, Show)

data QueueFamilyDatas = QueueFamilyDatas
  { graphicsQueue :: VkQueue
  , presentQueue :: VkQueue
  , queueFamilyCount :: Word32
  , queueFamilyIndicesPtr :: Ptr Word32
  , graphicsFamilyIndex :: Word32
  , presentFamilyIndex :: Word32
  } deriving (Eq, Show)

data SwapChainSupportDetails = SwapChainSupportDetails
  { capabilities :: VkSurfaceCapabilitiesKHR
  , formats      :: [VkSurfaceFormatKHR]
  , presentModes :: [VkPresentModeKHR]
  } deriving (Eq, Show)

data SwapChainData = SwapChainData
  { swapChain :: VkSwapchainKHR
  , swapChainImages :: [VkImage]
  , swapChainImageFormat :: VkFormat
  , swapChainExtent :: VkExtent2D
  } deriving (Eq, Show)

data RenderData = RenderData
  { imageAvailableSemaphore :: VkSemaphore
  , renderFinishedSemaphore :: VkSemaphore
  , device :: VkDevice
  , swapChainData :: SwapChainData
  , queueFamilyDatas :: QueueFamilyDatas
  , imageIndexPtr :: Ptr Word32
  , commandBuffers :: [VkCommandBuffer]
  } deriving (Eq, Show)

  
getExtensionNames :: (Traversable t1, VulkanMarshal t) => [Char] -> t1 t -> IO (t1 String)
getExtensionNames extensionType availableExtensionArrayPtr = do
  availableExtensionNames <- mapM getExtensionName availableExtensionArrayPtr
  putStrLn $ "Available " ++ extensionType ++ " extensions : " ++ (show (length availableExtensionNames))
  --mapM (\extensionName -> putStrLn $ "\t" ++ extensionName) availableExtensionNames
  return availableExtensionNames
  where 
    getExtensionName extensionPtr = 
      let extensionNamePtr = plusPtr (unsafePtr extensionPtr) (fieldOffset @"extensionName" @VkExtensionProperties)
      in peekCString $ castPtr extensionNamePtr

getInstanceExtensionSupport :: IO [String]
getInstanceExtensionSupport = do
  availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> 
    throwingVK "vkEnumerateInstanceExtensionProperties error"
      $ vkEnumerateInstanceExtensionProperties VK_NULL_HANDLE counterPtr valueArrayPtr
  getExtensionNames "Instance" availableExtensionArrayPtr

getDeviceExtensionSupport :: VkPhysicalDevice -> IO [String]
getDeviceExtensionSupport physicalDevice = do
  availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> 
    throwingVK "vkEnumerateInstanceExtensionProperties error"
      $ vkEnumerateDeviceExtensionProperties physicalDevice VK_NULL_HANDLE counterPtr valueArrayPtr
  getExtensionNames "Device" availableExtensionArrayPtr

checkExtensionSupport :: [String] -> [CString] -> IO Bool
checkExtensionSupport availableDeviceExtensions requireExtensions = do
  requireExtensionNames <- mapM peekCString requireExtensions    
  putStrLn $ "Require Extensions: " ++ show (length requireExtensionNames) ++ " / " ++ show (length availableDeviceExtensions) ++ " availables."
  isAvailable requireExtensionNames
  return . null $ requireExtensionNames \\ availableDeviceExtensions
  where 
    isAvailable [] = return ()
    isAvailable (x:xs) = do
      if elem x availableDeviceExtensions
        then putStrLn ("\t" ++ x ++ " (OK)")
        else putStrLn ("\t" ++ x ++ " (Failed)")
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
    throwingVK "vkCreateInstance: Failed to create vkInstance."
      $ vkCreateInstance (unsafePtr instanceCreateInfo) VK_NULL vkInstPtr    
    peek vkInstPtr  
  touchVkData instanceCreateInfo
  return vkInstance

destroyVulkanInstance :: VkInstance -> IO ()
destroyVulkanInstance vkInstance = do  
  putStrLn "Destroy VulkanInstance"
  vkDestroyInstance vkInstance VK_NULL  

createVkSurface :: Ptr vkInstance -> GLFW.Window -> IO VkSurfaceKHR
createVkSurface vkInstance window = do
  vkSurface <- alloca $ \vkSurfacePtr -> do
    throwingVK "glfwCreateWindowSurface: failed to create window surface"
      $ GLFW.createWindowSurface vkInstance window VK_NULL_HANDLE vkSurfacePtr
    putStrLn $ "Createad surface: " ++ show vkSurfacePtr
    peek vkSurfacePtr    
  return vkSurface

destroyVkSurface :: VkInstance -> VkSurfaceKHR -> IO ()
destroyVkSurface vkInstance vkSurface = do
  destroySurfaceFunc <- vkGetInstanceProc @VkDestroySurfaceKHR vkInstance
  destroySurfaceFunc vkInstance vkSurface VK_NULL_HANDLE
  putStrLn "Destroy VkSurfaceKHR"

querySwapChainSupport :: VkPhysicalDevice -> VkSurfaceKHR -> IO SwapChainSupportDetails
querySwapChainSupport physicalDevice vkSurface = do
  capabilities <- newVkData
    $ throwingVK "vkGetPhysicalDeviceSurfaceCapabilitiesKHR error"
    . vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice vkSurface
  formats <- asListVK $ \counterPtr valuePtr ->
    throwingVK "vkGetPhysicalDeviceSurfaceFormatsKHR error"
      $ vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice vkSurface counterPtr valuePtr
  presentModes <- asListVK $ \counterPtr valuePtr ->
    throwingVK "vkGetPhysicalDeviceSurfacePresentModesKHR error"
      $ vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice vkSurface counterPtr valuePtr
  return SwapChainSupportDetails {..}

isDeviceSuitable :: Maybe VkSurfaceKHR -> VkPhysicalDevice -> IO (Maybe SwapChainSupportDetails, Bool)
isDeviceSuitable maybeVkSurface physicalDevice = do
  deviceExtensionNames <- getDeviceExtensionSupport physicalDevice
  hasExtension <- checkExtensionSupport deviceExtensionNames requireDeviceExtensions
  (maybeSwapChainSupportDetails, result) <- case maybeVkSurface of
    Nothing -> pure (Nothing, True)
    Just vkSurface
      | not hasExtension -> pure (Nothing, False)
      | otherwise -> do
        swapChainSupportDetails@SwapChainSupportDetails {..} <- querySwapChainSupport physicalDevice vkSurface
        return (Just swapChainSupportDetails, not (null formats) && not (null presentModes))
  pure (maybeSwapChainSupportDetails, hasExtension && result)

getPhysicalDeviceFeatures :: IO VkPhysicalDeviceFeatures
getPhysicalDeviceFeatures = newVkData @VkPhysicalDeviceFeatures clearStorable

selectPhysicalDevice :: VkInstance 
  -> Maybe VkSurfaceKHR 
  -> IO (Maybe SwapChainSupportDetails, VkPhysicalDevice)
selectPhysicalDevice vkInstance maybeVkSurface = do
  devices <- asListVK $ \counterPtr valueArrayPtr ->
    throwingVK "pickPhysicalDevice: Failed to enumerate physical devices." 
      $ vkEnumeratePhysicalDevices vkInstance counterPtr valueArrayPtr
  when (null devices) $ throwVKMsg "Zeo device count!"
  putStrLn $ "Found " ++ show (length devices) ++ " devices."
  selectFirstSuitable devices
  where
    selectFirstSuitable [] = throwVKMsg "No suitable devices!"
    selectFirstSuitable (physicalDevice:physicalDeviceArray) = do
      (maybeSwapChainSupportDetails, result) <- isDeviceSuitable maybeVkSurface physicalDevice
      if result then do
          putStrLn $ "Selected physical device: " ++ show physicalDevice
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
      throwingVK "vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support."
        $ vkGetPhysicalDeviceSurfaceSupportKHR device queueFamilyIndex surface supportedPtr
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
  putStrLn $ "Found " ++ show familyCount ++ " queue families."
  queueFaimilies <- allocaArray familyCount $ \familiesPtr -> do
    vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueFamilyCountPtr familiesPtr
    zip [0..] <$> peekArray familyCount familiesPtr
  mapM_ (\(x,y) -> putStrLn $ "\t[" ++ (show x) ++ "] " ++ (show y) ) queueFaimilies
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
      { graphicsQueueIndex = defaultIndex
      , presentQueueIndex = getFamilyIndex presentationFamilyIndices defaultIndex
      , computeQueueIndex = getFamilyIndex computeFamilyIndices defaultIndex
      , transferQueueIndex = getFamilyIndex transferFamilyIndices defaultIndex
      , sparseBindingQueueIndex = getFamilyIndex sparseBindingFamilyIndices defaultIndex
      }    
  putStrLn $ "Graphics Queue Index : " ++ show (graphicsQueueIndex queueFamilyIndices)
  putStrLn $ "Presentation Queue Index : " ++ show (presentQueueIndex queueFamilyIndices) ++ " / " ++ show presentationFamilyIndices
  putStrLn $ "Computer Queue Index : " ++ show (computeQueueIndex queueFamilyIndices) ++ " / " ++ show computeFamilyIndices
  putStrLn $ "Transfer Queue Index : " ++ show (transferQueueIndex queueFamilyIndices) ++ " / " ++ show transferFamilyIndices
  putStrLn $ "Sparse Binding Queue Index : " ++ show (sparseBindingQueueIndex queueFamilyIndices)  ++ " / " ++ show sparseBindingFamilyIndices
  return queueFamilyIndices
  where
    getFamilyIndex [] _ = invalidQueueIndex
    getFamilyIndex indices defaultIndex = 
      let result = [x | x <- indices, x /= defaultIndex]
      in
        if isConcurrentMode && (elem defaultIndex indices) then defaultIndex
        else if 0 < (length result) then result !! 0
        else defaultIndex
        
getQueuePrioritiesPtr :: Float -> IO (Ptr Float)
getQueuePrioritiesPtr value = 
  alloca $ \ptr -> do
    poke ptr value
    return ptr

getQueueCreateInfos :: [Word32] -> Ptr Float -> IO [VkDeviceQueueCreateInfo]
getQueueCreateInfos queueFamilyIndices queuePrioritiesPtr = do
  queueCreateInfoList <- forM queueFamilyIndices $ \queueFamilyIndex ->
    newVkData @VkDeviceQueueCreateInfo $ \queueCreateInfoPtr -> do
        writeField @"sType" queueCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        writeField @"pNext" queueCreateInfoPtr VK_NULL_HANDLE
        writeField @"flags" queueCreateInfoPtr VK_ZERO_FLAGS
        writeField @"queueFamilyIndex" queueCreateInfoPtr queueFamilyIndex
        writeField @"queueCount" queueCreateInfoPtr 1
        writeField @"pQueuePriorities" queueCreateInfoPtr queuePrioritiesPtr
  return queueCreateInfoList

getDeviceCreateInfo :: 
  Ptr VkDeviceQueueCreateInfo
  -> Int
  -> VkPhysicalDeviceFeatures
  -> [String]
  -> Int
  -> Ptr CString
  -> IO VkDeviceCreateInfo
getDeviceCreateInfo 
  queueCreateInfoArrayPtr 
  queueCreateInfoCount 
  physicalDeviceFeatures 
  layers
  requireDeviceExtensionCount
  requireDeviceExtensionsPtr = do
  deviceCreateInfo <- withCStringList layers $ \layerCount layerNames -> do
    newVkData @VkDeviceCreateInfo $ \devCreateInfoPtr -> do
      writeField @"sType" devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
      writeField @"pNext" devCreateInfoPtr VK_NULL_HANDLE
      writeField @"flags" devCreateInfoPtr VK_ZERO_FLAGS
      writeField @"pQueueCreateInfos" devCreateInfoPtr queueCreateInfoArrayPtr
      writeField @"queueCreateInfoCount" devCreateInfoPtr (fromIntegral queueCreateInfoCount)
      writeField @"enabledLayerCount" devCreateInfoPtr (fromIntegral layerCount)
      writeField @"ppEnabledLayerNames" devCreateInfoPtr layerNames
      writeField @"enabledExtensionCount" devCreateInfoPtr (fromIntegral requireDeviceExtensionCount)
      writeField @"ppEnabledExtensionNames" devCreateInfoPtr requireDeviceExtensionsPtr
      writeField @"pEnabledFeatures" devCreateInfoPtr (unsafePtr physicalDeviceFeatures)
  return deviceCreateInfo

createQueues :: VkDevice -> [Word32] -> IO (Map.Map Word32 VkQueue)
createQueues device queueFamilyIndices = do
  queueList <- forM queueFamilyIndices $ \queueFamilyIndex -> do
    queue <- alloca $ \queuePtr -> do
      vkGetDeviceQueue device queueFamilyIndex 0 queuePtr
      peek queuePtr
    return (queueFamilyIndex, queue)
  putStrLn $ "Created Queues: " ++ show (length queueList)
  return $ Map.fromList queueList

createDevice :: VkDeviceCreateInfo -> VkPhysicalDevice -> IO VkDevice
createDevice deviceCreateInfo physicalDevice = do
  device <- alloca $ \devicePtr -> do
    throwingVK "vkCreateDevice: failed to create vkDevice"
      $ vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE devicePtr
    peek devicePtr
  putStrLn $ "Created Device: " ++ show device
  return device

destroyDevice :: VkDevice -> VkDeviceCreateInfo -> VkPhysicalDeviceFeatures -> IO ()
destroyDevice device deviceCreateInfo physicalDeviceFeatures = do
  putStrLn "Destroy VkDevice"
  vkDestroyDevice device VK_NULL_HANDLE
  touchVkData deviceCreateInfo
  touchVkData physicalDeviceFeatures

chooseSwapSurfaceFormat :: SwapChainSupportDetails -> IO VkSurfaceFormatKHR
chooseSwapSurfaceFormat swapChainSupportDetails = do
  if 1 == length(formats') && VK_FORMAT_UNDEFINED == getFormat (formats' !! 0) then 
    newVkData $ \surfaceFormatPtr -> do
      writeField @"format" surfaceFormatPtr VK_FORMAT_B8G8R8A8_UNORM
      writeField @"colorSpace" surfaceFormatPtr VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  else
    findAvailableFormat formats'
  where
    formats' = formats swapChainSupportDetails
    getFormat = getField @"format"
    getColorSpace = getField @"colorSpace"    
    findAvailableFormat :: [VkSurfaceFormatKHR] -> IO VkSurfaceFormatKHR
    findAvailableFormat [] = return $ formats' !! 0
    findAvailableFormat (x:xs) = do
      if VK_FORMAT_B8G8R8A8_UNORM == getFormat x && VK_COLOR_SPACE_SRGB_NONLINEAR_KHR == getColorSpace x
      then return x
      else findAvailableFormat xs

chooseSwapPresentMode :: SwapChainSupportDetails -> IO VkPresentModeKHR
chooseSwapPresentMode swapChainSupportDetails = do
  if elem VK_PRESENT_MODE_FIFO_KHR presentModes' then return VK_PRESENT_MODE_FIFO_KHR
  else if elem VK_PRESENT_MODE_MAILBOX_KHR presentModes' then return VK_PRESENT_MODE_MAILBOX_KHR
  else if elem VK_PRESENT_MODE_FIFO_RELAXED_KHR presentModes' then return VK_PRESENT_MODE_FIFO_RELAXED_KHR
  else if elem VK_PRESENT_MODE_IMMEDIATE_KHR presentModes' then return VK_PRESENT_MODE_IMMEDIATE_KHR
  else return VK_PRESENT_MODE_FIFO_KHR
  where
    presentModes' = presentModes swapChainSupportDetails

chooseSwapExtent :: SwapChainSupportDetails -> IO VkExtent2D
chooseSwapExtent swapChainSupportDetails = do
  imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
    writeField @"width" extentPtr $ max (width $ getField @"minImageExtent" capabilities')
                             $ min (width $ getField @"maxImageExtent" capabilities')
                                   (width $ getField @"currentExtent"  capabilities')
    writeField @"height" extentPtr $ max (height $ getField @"minImageExtent" capabilities')
                              $ min (height $ getField @"maxImageExtent" capabilities')
                                    (height $ getField @"currentExtent"  capabilities')
  return imageExtent
  where
    capabilities' = capabilities swapChainSupportDetails
    width = getField @"width"
    height = getField @"height"

createSwapChain :: VkDevice 
                -> Library.Vulkan.SwapChainSupportDetails 
                -> Word32 
                -> QueueFamilyDatas 
                -> VkSurfaceKHR 
                -> IO SwapChainData
createSwapChain device swapChainSupportDetails imageCount queueFamilyDatas vkSurface = do
  surfaceFormat <- chooseSwapSurfaceFormat swapChainSupportDetails
  presentMode <- chooseSwapPresentMode swapChainSupportDetails
  imageExtent <- chooseSwapExtent swapChainSupportDetails

  -- try tripple buffering
  let maxImageCount = getField @"maxImageCount" $ capabilities swapChainSupportDetails
      minImageCount = getField @"minImageCount" $ capabilities swapChainSupportDetails
      imageCount' = if maxImageCount <= 0
                   then max minImageCount imageCount
                   else min maxImageCount $ max minImageCount imageCount

  -- write VkSwapchainCreateInfoKHR
  swapChainCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swapChainCreateInfoPtr -> do
    writeField @"sType" swapChainCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
    writeField @"pNext" swapChainCreateInfoPtr VK_NULL_HANDLE
    writeField @"flags" swapChainCreateInfoPtr VK_ZERO_FLAGS
    writeField @"surface" swapChainCreateInfoPtr vkSurface
    writeField @"minImageCount" swapChainCreateInfoPtr (fromIntegral imageCount')
    writeField @"imageFormat" swapChainCreateInfoPtr (getField @"format" surfaceFormat)
    writeField @"imageColorSpace" swapChainCreateInfoPtr (getField @"colorSpace" surfaceFormat)
    writeField @"imageExtent" swapChainCreateInfoPtr imageExtent
    writeField @"imageArrayLayers" swapChainCreateInfoPtr 1
    writeField @"imageUsage" swapChainCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    if graphicsQueue queueFamilyDatas /= presentQueue queueFamilyDatas
    then do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_CONCURRENT
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr (queueFamilyCount queueFamilyDatas)
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr (queueFamilyIndicesPtr queueFamilyDatas)      
    else do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr 0
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr VK_NULL_HANDLE      
    writeField @"preTransform" swapChainCreateInfoPtr (getField @"currentTransform" $ capabilities swapChainSupportDetails)
    writeField @"compositeAlpha" swapChainCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
    writeField @"presentMode" swapChainCreateInfoPtr presentMode
    writeField @"clipped" swapChainCreateInfoPtr VK_TRUE
    writeField @"oldSwapchain" swapChainCreateInfoPtr VK_NULL_HANDLE
  putStrLn "Create SwapChain"
  putStrLn $ "\timageCount : " ++ (show imageCount')
  putStrLn $ "\timageFormat : " ++ (show $ getField @"imageFormat" swapChainCreateInfo)
  putStrLn $ "\timageColorSpace : " ++ (show $ getField @"imageColorSpace" swapChainCreateInfo)
  putStrLn $ "\timageExtent : " ++ (show $ getField @"imageExtent" swapChainCreateInfo)
  putStrLn $ "\timageSharingMode : " ++ (show $ getField @"imageSharingMode" swapChainCreateInfo)

  swapChain <- alloca $ \swapChainPtr -> do
    throwingVK "vkCreateSwapchainKHR failed!"
      $ vkCreateSwapchainKHR device (unsafePtr swapChainCreateInfo) VK_NULL_HANDLE swapChainPtr
    peek swapChainPtr
  swapChainImages <- asListVK $ \counterPtr valueArrayPtr ->
    throwingVK "vkGetSwapchainImagesKHR error"
      $ vkGetSwapchainImagesKHR device swapChain counterPtr valueArrayPtr
  let swapChainData = SwapChainData { swapChain = swapChain
                                    , swapChainImages = swapChainImages
                                    , swapChainImageFormat = (getField @"imageFormat" swapChainCreateInfo)
                                    , swapChainExtent = (getField @"imageExtent" swapChainCreateInfo) }
  touchVkData swapChainCreateInfo
  return swapChainData

destroySwapChain :: VkDevice -> VkSwapchainKHR -> IO ()
destroySwapChain device swapChain = do
  putStrLn "Destroy SwapChain"
  vkDestroySwapchainKHR device swapChain VK_NULL_HANDLE

createSwapChainImageViews :: VkDevice -> SwapChainData -> IO [VkImageView]
createSwapChainImageViews device swapChainData = do
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
      writeField @"format" viewPtr (swapChainImageFormat swapChainData)
      writeField @"components" viewPtr components
      writeField @"subresourceRange" viewPtr subresourceRange  
  imageViewCreateInfos <- mapM getImageViewCreateInfo (swapChainImages swapChainData)
  imageViews <- forM imageViewCreateInfos $ \imageViewCreateInfo ->
    alloca $ \imageViewPtr -> do
      throwingVK "vkCreateImageView error"
        $ vkCreateImageView device (unsafePtr imageViewCreateInfo) VK_NULL_HANDLE imageViewPtr
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
      &* set @"format" (swapChainImageFormat swapChainData)
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
      throwingVK "vkCreatePipelineLayout failed!"
        $ vkCreateRenderPass device (unsafePtr renderPassCreateInfo) VK_NULL renderPassPtr
      peek renderPassPtr
    touchVkData renderPassCreateInfo
    putStrLn $ "Create RenderPass: " ++ show (swapChainImageFormat swapChainData)
    return renderPass

destroyRenderPass :: VkDevice -> VkRenderPass -> IO ()
destroyRenderPass device renderPass = do
  putStrLn "Destroy RenderPass"
  vkDestroyRenderPass device renderPass VK_NULL


createPipelineLayout :: VkDevice -> IO VkPipelineLayout
createPipelineLayout device = do
  putStrLn "Create PipelineLayout"
  pipelineLayout <- alloca $ \pipelineLayoutPtr -> do
    throwingVK "vkCreatePipelineLayout failed!"
      $ vkCreatePipelineLayout device (unsafePtr pipelineCreateInfo) VK_NULL pipelineLayoutPtr
    peek pipelineLayoutPtr
  touchVkData pipelineCreateInfo
  return pipelineLayout
  where
    pipelineCreateInfo :: VkPipelineLayoutCreateInfo
    pipelineCreateInfo = createVk @VkPipelineLayoutCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"setLayoutCount" 0      
      &* set @"pSetLayouts" VK_NULL
      &* set @"pushConstantRangeCount" 0      
      &* set @"pPushConstantRanges" VK_NULL

destroyPipelineLayout :: VkDevice -> VkPipelineLayout -> IO ()
destroyPipelineLayout device pipelineLayout = do
  putStrLn "Destroy PipelineLayout"
  vkDestroyPipelineLayout device pipelineLayout VK_NULL


createGraphicsPipeline :: VkDevice
                       -> SwapChainData
                       -> [VkPipelineShaderStageCreateInfo]
                       -> VkRenderPass
                       -> VkPipelineLayout
                       -> IO VkPipeline
createGraphicsPipeline device swapChainData shaderStageInfos renderPass pipelineLayout =  
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
      &* set @"width" (fromIntegral $ getField @"width" (swapChainExtent swapChainData))
      &* set @"height" (fromIntegral $ getField @"height" (swapChainExtent swapChainData))
      &* set @"minDepth" 0
      &* set @"maxDepth" 1

    scissorRect :: VkRect2D
    scissorRect = createVk @VkRect2D
      $  set   @"extent" (swapChainExtent swapChainData)
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

    getGraphicsPipelineCreateInfo :: Ptr VkPipelineShaderStageCreateInfo -> VkGraphicsPipelineCreateInfo
    getGraphicsPipelineCreateInfo shaderStageInfosPtr = createVk @VkGraphicsPipelineCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"stageCount" (fromIntegral $ length shaderStageInfos)
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
  in do
    putStrLn $ "Create GraphicsPipeline: " ++ show (swapChainExtent swapChainData)
    shaderStageInfosPtr <- newArray shaderStageInfos
    let graphicsPipelineCreateInfo = getGraphicsPipelineCreateInfo shaderStageInfosPtr
    createGraphicsPipelinesFunc <- vkGetDeviceProc @VkCreateGraphicsPipelines device    
    graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
      throwingVK "vkCreateGraphicsPipelines failed!"
        $ createGraphicsPipelinesFunc device VK_NULL_HANDLE 1 (unsafePtr graphicsPipelineCreateInfo) VK_NULL graphicsPipelinePtr
      peek graphicsPipelinePtr
    touchVkData graphicsPipelineCreateInfo
    free shaderStageInfosPtr
    return graphicsPipeline

destroyGraphicsPipeline :: VkDevice -> VkPipeline -> IO ()
destroyGraphicsPipeline device graphicsPipeline = do
  putStrLn $ "Destroy GraphicsPipeline"
  vkDestroyPipeline device graphicsPipeline VK_NULL


createFramebuffers :: VkDevice -> VkRenderPass -> SwapChainData -> [VkImageView] -> IO [VkFramebuffer]
createFramebuffers device renderPass swapChainData swapChainImageViews = do
  putStrLn $ "Create Framebuffers: " ++ show (length swapChainImageViews)
  framebuffers <- mapM createFrameBuffer swapChainImageViews  
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
            &* set @"width" (getField @"width" (swapChainExtent swapChainData))
            &* set @"height" (getField @"height" (swapChainExtent swapChainData))
            &* set @"layers" 1
      in do
        frameBuffer <- alloca $ \framebufferPtr -> do
          throwingVK "vkCreateFramebuffer failed!"
            $ vkCreateFramebuffer device (unsafePtr frameBufferCreateInfo) VK_NULL framebufferPtr
          peek framebufferPtr
        touchVkData frameBufferCreateInfo
        return frameBuffer

destroyFramebuffers :: VkDevice -> [VkFramebuffer] -> IO ()
destroyFramebuffers device frameBuffers = do
  putStrLn $ "Destroy Framebuffers"
  forM_ frameBuffers $ \frameBuffer ->
    vkDestroyFramebuffer device frameBuffer VK_NULL_HANDLE


createCommandPool :: VkDevice -> QueueFamilyDatas -> IO VkCommandPool
createCommandPool device queueFamilyDatas = do
  putStrLn $ "Create Command Pool: graphicsFamilyIndex(" ++ show (graphicsFamilyIndex queueFamilyDatas) ++ ")"
  let commandPoolCreateInfo = createVk @VkCommandPoolCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"queueFamilyIndex" (graphicsFamilyIndex queueFamilyDatas)
  commandPool <- alloca $ \commandPoolPtr -> do
    withPtr commandPoolCreateInfo $ \createInfoPtr -> do
      throwingVK "vkCreateCommandPool failed!"
        $ vkCreateCommandPool device createInfoPtr VK_NULL commandPoolPtr
    peek commandPoolPtr
  return commandPool

destroyCommandPool :: VkDevice -> VkCommandPool -> IO ()
destroyCommandPool device commandPool = do
  putStrLn $ "Destroy Command Pool"
  vkDestroyCommandPool device commandPool VK_NULL


createCommandBuffers :: VkDevice
                     -> VkPipeline
                     -> VkCommandPool
                     -> VkRenderPass
                     -> SwapChainData
                     -> [VkFramebuffer]
                     -> IO ([VkCommandBuffer], Ptr VkCommandBuffer)
createCommandBuffers device pipeline commandPool renderPass SwapChainData{..} frameBuffers = do
  let buffersCount = length frameBuffers
  (commandBuffers, commandBuffersPtr) <- allocaArray buffersCount $ \commandBuffersPtr -> do    
    let allocationInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"commandPool" commandPool
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandBufferCount" (fromIntegral buffersCount)
    withPtr allocationInfo $ \allocationInfoPtr ->
      throwingVK "vkAllocateCommandBuffers failed!"
        $ vkAllocateCommandBuffers device allocationInfoPtr commandBuffersPtr
    commandBuffers <- peekArray buffersCount commandBuffersPtr
    putStrLn $ "Create Command Buffer: "  ++ show buffersCount
    return (commandBuffers, commandBuffersPtr)

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
            &* set @"extent" swapChainExtent )
          &* set @"clearValueCount" 1
          &* setVkRef @"pClearValues"
              ( createVk $ setVk @"color"
                $  setAt @"float32" @0 0
                &* setAt @"float32" @1 0
                &* setAt @"float32" @2 0.2
                &* setAt @"float32" @3 1 )
    -- begin commands
    putStrLn $ "\tvkBeginCommandBuffer: " ++ show commandBuffer
    withPtr commandBufferBeginInfo $ \commandBufferBeginInfoPtr -> do
      throwingVK "vkBeginCommandBuffer failed!"
        $ vkBeginCommandBuffer commandBuffer commandBufferBeginInfoPtr
    withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
      vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE
    -- basic drawing commands
    vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
    vkCmdDraw commandBuffer 3 1 0 0
    -- finishing up
    vkCmdEndRenderPass commandBuffer
    throwingVK "vkEndCommandBuffer failed!" 
      $ vkEndCommandBuffer commandBuffer
  return (commandBuffers, commandBuffersPtr)

destroyCommandBuffers :: VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()
destroyCommandBuffers device commandPool buffersCount commandBuffersPtr =
  vkFreeCommandBuffers device commandPool buffersCount commandBuffersPtr


createSemaphore :: VkDevice -> IO VkSemaphore
createSemaphore device = do  
  semaphore <- alloca $ \semaphorePtr -> do
    let semaphoreCreateInfo = createVk @VkSemaphoreCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
    withPtr semaphoreCreateInfo $ \semaphoreCreateInfoPtr ->
      throwingVK "vkCreateSemaphore failed!"
        $ vkCreateSemaphore device semaphoreCreateInfoPtr VK_NULL semaphorePtr
    peek semaphorePtr
  putStrLn $ "Create Semaphore: " ++ show semaphore
  return semaphore

destroySemaphore :: VkDevice -> VkSemaphore -> IO ()
destroySemaphore device semaphore = do
  putStrLn $ "Destroy Semaphore: " ++ show semaphore
  vkDestroySemaphore device semaphore VK_NULL


drawFrame :: RenderData -> IO ()
drawFrame RenderData {..} =
  withArray commandBuffers $ \commandBuffersPtr -> do
    throwingVK "vkAcquireNextImageKHR failed!"
      $ vkAcquireNextImageKHR device swapChain maxBound imageAvailableSemaphore VK_NULL_HANDLE imageIndexPtr

    commandBufferPtr <- (\imageIndex -> plusPtr commandBuffersPtr (fromIntegral imageIndex * sizeOf (undefined :: VkCommandBuffer))) 
        <$> peek imageIndexPtr

    withPtr (submitInfo commandBufferPtr) $ \submitInfoPtr ->
      throwingVK "vkQueueSubmit failed!"
        $ vkQueueSubmit graphicsQueue 1 submitInfoPtr VK_NULL

    withPtr presentInfo $ \presentInfoPtr ->
      throwingVK "vkQueuePresentKHR failed!"
        $ vkQueuePresentKHR presentQueue presentInfoPtr

    throwingVK "vkQueueWaitIdle failed!"
      $ vkQueueWaitIdle presentQueue
  where
    SwapChainData {..} = swapChainData
    QueueFamilyDatas {..} = queueFamilyDatas
    submitInfo :: Ptr VkCommandBuffer -> VkSubmitInfo
    submitInfo commandBufferPtr = createVk @VkSubmitInfo
      $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
      &* set @"pNext" VK_NULL
      &* set @"waitSemaphoreCount" 1
      &* setListRef @"pWaitSemaphores" [imageAvailableSemaphore]
      &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
      &* set @"commandBufferCount" 1
      &* set @"pCommandBuffers" commandBufferPtr
      &* set @"signalSemaphoreCount" 1
      &* setListRef @"pSignalSemaphores" [renderFinishedSemaphore]
    presentInfo :: VkPresentInfoKHR
    presentInfo = createVk @VkPresentInfoKHR
      $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
      &* set @"pNext" VK_NULL
      &* set @"pImageIndices" imageIndexPtr
      &* set @"waitSemaphoreCount" 1
      &* setListRef @"pWaitSemaphores" [renderFinishedSemaphore]
      &* set @"swapchainCount" 1
      &* setListRef @"pSwapchains" [swapChain]

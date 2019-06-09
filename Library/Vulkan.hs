{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
    ( vulkanLayers
    , requireDeviceExtensions
    , QueueFamilyIndices (..)
    , QueueFamilyDatas (..)
    , SwapChainDatas (..)
    , SwapChainSupportDetails (..)
    , getInstanceExtensionSupport
    , getDeviceExtensionSupport
    , checkExtensionSupport
    , getApplicationInfo
    , getInstanceCreateInfo
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
    , getSwapChainCreateInfo
    , createSwapChain
    , destroySwapChain
    , getSwapChainImageViewCreateInfos
    , createSwapChainImageViews
    , destroySwapChainImageViews
    ) where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Semigroup
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
import Lib.Utils

vulkanLayers :: [String]
vulkanLayers = ["VK_LAYER_LUNARG_standard_validation"]

requireDeviceExtensions :: [CString]
requireDeviceExtensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

invalidQueueIndex :: Word32
invalidQueueIndex = -1

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

data SwapChainDatas = SwapChainDatas
  { swapChain :: VkSwapchainKHR
  , swapChainImages :: [VkImage]
  , swapChainImageFormat :: VkFormat
  , swapChainExtent :: VkExtent2D
  } deriving (Eq, Show)

  
getExtensionNames :: (Traversable t1, VulkanMarshal t) => [Char] -> t1 t -> IO (t1 String)
getExtensionNames extensionType availableExtensionArrayPtr = do
  availableExtensionNames <- mapM getExtensionName availableExtensionArrayPtr
  --putStrLn $ "Available " ++ extensionType ++ " extensions : " ++ (show (length availableExtensionNames))
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

getApplicationInfo :: String -> String -> VkApplicationInfo
getApplicationInfo progName engineName = createVk @VkApplicationInfo
    $ set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
    &* set @"pNext" VK_NULL
    &* setStrRef @"pApplicationName" progName
    &* set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
    &* setStrRef @"pEngineName" engineName
    &* set @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
    &* set @"apiVersion" (_VK_MAKE_VERSION 1 0 0)

getInstanceCreateInfo :: VkApplicationInfo -> [String] -> [CString] -> VkInstanceCreateInfo
getInstanceCreateInfo applicatonInfo layers extensions = createVk @VkInstanceCreateInfo
  $ set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  &* set @"pNext" VK_NULL
  &* set @"flags" 0
  &* setVkRef @"pApplicationInfo" applicatonInfo
  &* set @"enabledLayerCount" (fromIntegral $ length layers)
  &* setStrListRef @"ppEnabledLayerNames" layers
  &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
  &* setListRef @"ppEnabledExtensionNames" extensions

createVulkanInstance :: VkInstanceCreateInfo -> IO VkInstance
createVulkanInstance instanceCreateInfo  = do
  vkInstance <- alloca $ \vkInstPtr -> do
    throwingVK "vkCreateInstance: Failed to create vkInstance."
      $ vkCreateInstance (unsafePtr instanceCreateInfo) VK_NULL vkInstPtr    
    peek vkInstPtr  
  return vkInstance

destroyVulkanInstance :: VkInstanceCreateInfo -> VkInstance -> IO ()
destroyVulkanInstance instanceCreateInfo vkInstance = do  
  putStrLn "Destroy VulkanInstance"
  vkDestroyInstance vkInstance VK_NULL
  touchVkData instanceCreateInfo   

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
selectPresentationFamily device surface ((queueFamilyIndex, queueFamilyProperty):xs) = do
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
  mapM (\(x,y) -> putStrLn $ "\t[" ++ (show x) ++ "] " ++ (show y) ) queueFaimilies
  return queueFaimilies

getQueueFamilyIndices :: VkPhysicalDevice -> VkSurfaceKHR -> IO QueueFamilyIndices
getQueueFamilyIndices physicalDevice vkSurface = do 
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
  putStrLn $ "Presentation Queue Index : " ++ show (presentQueueIndex queueFamilyIndices)
  putStrLn $ "Computer Queue Index : " ++ show (computeQueueIndex queueFamilyIndices)
  putStrLn $ "Transfer Queue Index : " ++ show (transferQueueIndex queueFamilyIndices)
  putStrLn $ "Sparse Binding Queue Index : " ++ show (sparseBindingQueueIndex queueFamilyIndices)  
  return queueFamilyIndices
  where
    getFamilyIndex [] _ = invalidQueueIndex
    getFamilyIndex indices defaultIndex = 
      let result = [x | x <- indices, x /= defaultIndex]
      in
        if 0 < (length result)
        then result !! 0
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
        writeField @"flags" queueCreateInfoPtr 0
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
      writeField @"flags" devCreateInfoPtr 0
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
  vkDevice <- alloca $ \vkDevicePtr -> do
    throwingVK "vkCreateDevice: failed to create vkDevice"
      $ vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE vkDevicePtr
    peek vkDevicePtr
  putStrLn $ "Created Device: " ++ show vkDevice
  return vkDevice

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

getSwapChainCreateInfo :: Library.Vulkan.SwapChainSupportDetails -> Word32 -> QueueFamilyDatas -> VkSurfaceKHR -> IO VkSwapchainCreateInfoKHR
getSwapChainCreateInfo swapChainSupportDetails imageCount queueFamilyDatas vkSurface = do
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
    writeField @"flags" swapChainCreateInfoPtr 0
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
  return swapChainCreateInfo

createSwapChain :: VkDevice -> VkSwapchainCreateInfoKHR -> IO SwapChainDatas
createSwapChain vkDevice swapChainCreateInfo = do
  swapChain <- alloca $ \swapChainPtr -> do
    throwingVK "vkCreateSwapchainKHR failed!"
      $ vkCreateSwapchainKHR vkDevice (unsafePtr swapChainCreateInfo) VK_NULL_HANDLE swapChainPtr
    peek swapChainPtr
  swapChainImages <- asListVK $ \counterPtr valueArrayPtr ->
    throwingVK "vkGetSwapchainImagesKHR error"
      $ vkGetSwapchainImagesKHR vkDevice swapChain counterPtr valueArrayPtr
  return SwapChainDatas
    { swapChain = swapChain
    , swapChainImages = swapChainImages
    , swapChainImageFormat = (getField @"imageFormat" swapChainCreateInfo)
    , swapChainExtent = (getField @"imageExtent" swapChainCreateInfo) }

destroySwapChain :: VkDevice -> VkSwapchainCreateInfoKHR -> VkSwapchainKHR -> IO ()
destroySwapChain vkDevice swapChainCreateInfo swapChain = do
  putStrLn "Destroy SwapChain"
  vkDestroySwapchainKHR vkDevice swapChain VK_NULL_HANDLE
  touchVkData swapChainCreateInfo

getSwapChainImageViewCreateInfos :: SwapChainDatas -> IO [VkImageViewCreateInfo]
getSwapChainImageViewCreateInfos swapChainDatas = do
    components <- newVkData $ \componentsPtr -> do
      writeField @"r" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
      writeField @"g" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
      writeField @"b" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
      writeField @"a" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
    subresourceRange <- newVkData $ \subresourceRangePtr -> do
      writeField @"aspectMask" subresourceRangePtr VK_IMAGE_ASPECT_COLOR_BIT
      writeField @"baseMipLevel" subresourceRangePtr 0
      writeField @"levelCount" subresourceRangePtr 1
      writeField @"baseArrayLayer" subresourceRangePtr 0
      writeField @"layerCount" subresourceRangePtr 1
    mapM (getImageViewCreateInfo components subresourceRange) (swapChainImages swapChainDatas)
  where
    getImageViewCreateInfo components subresourceRange image = newVkData @VkImageViewCreateInfo $ \viewPtr -> do
      writeField @"sType" viewPtr VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      writeField @"pNext" viewPtr VK_NULL_HANDLE
      writeField @"flags" viewPtr 0
      writeField @"image" viewPtr image
      writeField @"viewType" viewPtr VK_IMAGE_VIEW_TYPE_2D
      writeField @"format" viewPtr (swapChainImageFormat swapChainDatas)
      writeField @"components" viewPtr components
      writeField @"subresourceRange" viewPtr subresourceRange

createSwapChainImageViews :: VkDevice -> [VkImageViewCreateInfo] -> IO [VkImageView]
createSwapChainImageViews device imageViewCreateInfos = do
  imageViews <- forM imageViewCreateInfos $ \imageViewCreateInfo ->
    alloca $ \imageViewPtr -> do
      throwingVK "vkCreateImageView error"
        $ vkCreateImageView device (unsafePtr imageViewCreateInfo) VK_NULL_HANDLE imageViewPtr
      peek imageViewPtr
  return imageViews

destroySwapChainImageViews :: VkDevice -> [VkImageViewCreateInfo] -> [VkImageView] -> IO ()
destroySwapChainImageViews device imageViewCreateInfos imageViews = do
  mapM_ (flip (vkDestroyImageView device) VK_NULL_HANDLE) imageViews
  mapM_ touchVkData imageViewCreateInfos
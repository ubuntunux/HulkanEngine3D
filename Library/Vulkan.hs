{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
    ( vulkanLayers
    , requireDeviceExtensions
    , QueueFamilyDatas (..)
    , SwapChainImgInfo (..)
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
    , getQueueFamilyInfos
    , getQueuePrioritiesPtr
    , getQueueCreateInfos
    , getDeviceCreateInfo
    , createQueues
    , createDevice
    , destroyDevice
    , withSwapChain
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

data SwapChainImgInfo = SwapChainImgInfo
  { swapchain   :: VkSwapchainKHR
  , swImgs      :: [VkImage]
  , swImgFormat :: VkFormat
  , swExtent    :: VkExtent2D
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

destroyVulkanInstance :: VkInstance -> IO ()
destroyVulkanInstance vkInstance = do  
  vkDestroyInstance vkInstance VK_NULL
  putStrLn "Destroy VulkanInstance"

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

getQueueFamilyInfos :: VkPhysicalDevice -> VkSurfaceKHR -> IO ([Word32], [VkQueueFamilyProperties])
getQueueFamilyInfos physicalDevice vkSurface = do 
  queueFaimilies <- getQueueFamilies physicalDevice  
  graphicsQueueIndices <- selectQueueFamily VK_QUEUE_GRAPHICS_BIT queueFaimilies
  transferFamilyIndices <- selectQueueFamily VK_QUEUE_TRANSFER_BIT queueFaimilies
  computeFamilyIndices <- selectQueueFamily VK_QUEUE_COMPUTE_BIT queueFaimilies
  presentationFamilyIndices <- selectPresentationFamily physicalDevice vkSurface queueFaimilies  
  let
    graphicsQueueIndex = graphicsQueueIndices !! 0
    transferFamilyIndex = getFamilyIndex transferFamilyIndices graphicsQueueIndex
    computeFamilyIndex = getFamilyIndex computeFamilyIndices graphicsQueueIndex
    presentationFamilyIndex = presentationFamilyIndices !! 0
    graphicsQueueProperty = snd $ queueFaimilies !! (fromIntegral graphicsQueueIndex)
    presentationQueueProperty = snd $ queueFaimilies !! (fromIntegral presentationFamilyIndex)
    (queueFamilyIndices, queueFamilyProperties) = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([], []) queueFaimilies
  putStrLn $ "Graphics Queue Index : " ++ show graphicsQueueIndex
  putStrLn $ "Transfer Queue Index : " ++ show transferFamilyIndex
  putStrLn $ "Computer Queue Index : " ++ show computeFamilyIndex
  putStrLn $ "Presentation Queue Index : " ++ show presentationFamilyIndex
  return (queueFamilyIndices, queueFamilyProperties)
  --return ([graphicsQueueIndex, presentationFamilyIndex], [graphicsQueueProperty, presentationQueueProperty])  
  where
    getFamilyIndex [] defaultIndex = defaultIndex
    getFamilyIndex indices defaultIndex = [x | x <- indices, x /= defaultIndex] !! 0

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

createQueues :: VkDevice -> [Word32] -> IO [VkQueue]
createQueues device queueFamilyIndices = do
  queues <- forM queueFamilyIndices $ \queueFamilyIndex -> do
    queue <- alloca $ \queuePtr -> do
      vkGetDeviceQueue device queueFamilyIndex 0 queuePtr
      peek queuePtr
    return queue
  putStrLn $ "Created Queues: " ++ show (length queues)
  return queues

createDevice :: VkDeviceCreateInfo -> VkPhysicalDevice -> IO VkDevice
createDevice deviceCreateInfo physicalDevice = do
  vkDevice <- alloca $ \vkDevicePtr -> do
    throwingVK "vkCreateDevice: failed to create vkDevice"
      $ vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE vkDevicePtr
    peek vkDevicePtr
  putStrLn $ "Created Device: " ++ show vkDevice
  return vkDevice

destroyDevice :: VkDevice -> IO ()
destroyDevice device = vkDestroyDevice device VK_NULL_HANDLE

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
      if VK_FORMAT_B8G8R8A8_UNORM == getFormat x && VK_COLOR_SPACE_SRGB_NONLINEAR_KHR == getColorSpace x then 
        return x
      else
        findAvailableFormat xs

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

withSwapChain :: VkDevice
              -> SwapChainSupportDetails
              -> QueueFamilyDatas
              -> VkSurfaceKHR
              -> (SwapChainImgInfo -> IO a)
              -> IO a
withSwapChain vkDevice swapChainSupportDetails queueFamilyDatas vkSurface action = do
  surfaceFormat <- chooseSwapSurfaceFormat swapChainSupportDetails
  presentMode <- chooseSwapPresentMode swapChainSupportDetails
  imageExtent <- chooseSwapExtent swapChainSupportDetails

  -- try tripple buffering
  let maxIC = getField @"maxImageCount" $ capabilities swapChainSupportDetails
      minIC = getField @"minImageCount" $ capabilities swapChainSupportDetails
      imageCount = if maxIC <= 0
                   then max minIC 3
                   else min maxIC $ max minIC 3

  -- write VkSwapchainCreateInfoKHR
  swCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swCreateInfoPtr -> do
    writeField @"sType" swCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
    writeField @"pNext" swCreateInfoPtr VK_NULL_HANDLE
    writeField @"flags" swCreateInfoPtr 0
    writeField @"surface" swCreateInfoPtr vkSurface
    writeField @"minImageCount" swCreateInfoPtr imageCount
    writeField @"imageFormat" swCreateInfoPtr (getField @"format" surfaceFormat)
    writeField @"imageColorSpace" swCreateInfoPtr (getField @"colorSpace" surfaceFormat)
    writeField @"imageExtent" swCreateInfoPtr imageExtent
    writeField @"imageArrayLayers" swCreateInfoPtr 1
    writeField @"imageUsage" swCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    if graphicsQueue queueFamilyDatas /= presentQueue queueFamilyDatas
    then do
      writeField @"imageSharingMode" swCreateInfoPtr VK_SHARING_MODE_CONCURRENT
      writeField @"queueFamilyIndexCount" swCreateInfoPtr (queueFamilyCount queueFamilyDatas)
      writeField @"pQueueFamilyIndices" swCreateInfoPtr (queueFamilyIndicesPtr queueFamilyDatas)
    else do
      writeField @"imageSharingMode" swCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
      writeField @"queueFamilyIndexCount" swCreateInfoPtr 0
      writeField @"pQueueFamilyIndices" swCreateInfoPtr VK_NULL_HANDLE
    writeField @"preTransform" swCreateInfoPtr (getField @"currentTransform" $ capabilities swapChainSupportDetails)
    writeField @"compositeAlpha" swCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
    writeField @"presentMode" swCreateInfoPtr presentMode
    writeField @"clipped" swCreateInfoPtr VK_TRUE
    writeField @"oldSwapchain" swCreateInfoPtr VK_NULL_HANDLE

  swapChain <- alloca $ \swPtr -> do
    throwingVK "vkCreateSwapchainKHR failed!"
      $ vkCreateSwapchainKHR vkDevice (unsafePtr swCreateInfo) VK_NULL_HANDLE swPtr
    peek swPtr

  swImgs <- asListVK $ \x ->
    throwingVK "vkGetSwapchainImagesKHR error"
      . vkGetSwapchainImagesKHR vkDevice swapChain x

  let swInfo = SwapChainImgInfo
        { swapchain   = swapChain
        , swImgs      = swImgs
        , swImgFormat = getField @"format" surfaceFormat
        , swExtent    = imageExtent }

  finally (action swInfo) $ do
    vkDestroySwapchainKHR vkDevice swapChain VK_NULL_HANDLE
    touchVkData swCreateInfo


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
    ( getInstanceExtensionSupport
    , getDeviceExtensionSupport
    , checkExtensionSupport
    , getApplicationInfo
    , getInstanceCreateInfo
    , createVulkanInstance
    , destroyVulkanInstance
    , selectPhysicalDevice    
    , getPhysicalDeviceFeatures
    , getQueueFamilyMap
    , getQueuePrioritiesPtr
    , getQueueCreateInfo
    , getDeviceCreateInfo
    , createQueues
    , createDevice
    , destroyDevice
    , touchVKDatas
    ) where

import Control.Exception
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
import Lib.Utils
import Lib.Vulkan hiding (isDeviceSuitable)


touchVKDatas :: (VulkanMarshal a2, VulkanMarshal a1, VulkanMarshal a) 
  => a2 -> a1 -> a -> IO ()
touchVKDatas instanceCreateInfo deviceCreateInfo physicalDeviceFeatures = do
  touchVkData instanceCreateInfo 
  touchVkData deviceCreateInfo 
  touchVkData physicalDeviceFeatures 

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
  putStrLn $ "RequireExtension: " ++ show (length requireExtensionNames)
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
destroyVulkanInstance vkInstance = vkDestroyInstance vkInstance VK_NULL

isDeviceSuitable :: Maybe VkSurfaceKHR -> VkPhysicalDevice -> IO (Maybe SwapChainSupportDetails, Bool)
isDeviceSuitable maybeVkSurface physicalDevice = do
  deviceExtensionNames <- getDeviceExtensionSupport physicalDevice
  hasExtension <- checkExtensionSupport deviceExtensionNames [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
  (maybeSwapChainSupportDetails, result) <- case maybeVkSurface of
    Nothing -> pure (Nothing, True)
    Just vkSurface
      | not hasExtension -> pure (Nothing, False)
      | otherwise -> do
        swapChainSupportDetails@SwapChainSupportDetails {..} <- querySwapChainSupport physicalDevice vkSurface
        return (Just swapChainSupportDetails, 
          not (null formats) && not (null presentModes))
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

selectQueueFamily :: [Word32] -> VkQueueBitmask FlagMask -> [(Word32, VkQueueFamilyProperties)] -> (Word32, VkQueueFamilyProperties)
selectQueueFamily ignoreList requireQueueFlags [] = throw $ VulkanException Nothing "selectQueueFamily: not found!"
selectQueueFamily ignoreList requireQueueFlags (queueFamily@(queueFamilyIndex, queueFamilyProperty):xs) =
  if not (elem queueFamilyIndex ignoreList) && queueCount > 0 && (queueFlags .&. requireQueueFlags) /= zeroBits
    then queueFamily
    else selectQueueFamily ignoreList requireQueueFlags xs
  where
    queueCount = getField @"queueCount" queueFamilyProperty
    queueFlags = getField @"queueFlags" queueFamilyProperty

selectGraphicsFamily :: [Word32] -> [(Word32, VkQueueFamilyProperties)] -> IO (Word32, VkQueueFamilyProperties)
selectGraphicsFamily ignoreList queueFaimilies = do
  return $ selectQueueFamily ignoreList VK_QUEUE_GRAPHICS_BIT queueFaimilies

selectTransferFamily :: [Word32] -> [(Word32, VkQueueFamilyProperties)] -> IO (Word32, VkQueueFamilyProperties)
selectTransferFamily ignoreList queueFaimilies = do
  return $ selectQueueFamily ignoreList VK_QUEUE_TRANSFER_BIT queueFaimilies

selectComputeFamily :: [Word32] -> [(Word32, VkQueueFamilyProperties)] -> IO (Word32, VkQueueFamilyProperties)
selectComputeFamily ignoreList queueFaimilies = do
  return $ selectQueueFamily ignoreList VK_QUEUE_COMPUTE_BIT queueFaimilies

selectPresentationFamily :: VkPhysicalDevice
    -> VkSurfaceKHR
    -> [(Word32, VkQueueFamilyProperties)]
    -> IO (Word32, VkQueueFamilyProperties)
selectPresentationFamily _ _ [] = throw $ VulkanException Nothing "selectPresentFamily: not Found"
selectPresentationFamily device surface (x@(queueFamilyIndex, queueFamilyProperty):xs)
    | (getField @"queueCount" queueFamilyProperty) <= 0 = selectGraphicsFamily [] xs
    | otherwise = do
      supported <- alloca $ \supportedPtr -> do
        throwingVK "vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support."
          $ vkGetPhysicalDeviceSurfaceSupportKHR device queueFamilyIndex surface supportedPtr
        peek supportedPtr
      if VK_TRUE == supported
      then pure x
      else selectPresentationFamily device surface xs

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

getQueueFamilyMap :: VkPhysicalDevice -> VkSurfaceKHR -> IO (Map.Map Word32 VkQueueFamilyProperties)
getQueueFamilyMap physicalDevice vkSurface = do 
  queueFaimilies <- getQueueFamilies physicalDevice
  graphicsQueue@(index0, properties0) <- selectGraphicsFamily [] queueFaimilies
  transferFamily@(index1, properties1) <- selectTransferFamily [index0] queueFaimilies
  computeFamily@(index2, properties2) <- selectComputeFamily [index0, index1] queueFaimilies
  presentationFamily@(index3, properties3) <- selectPresentationFamily physicalDevice vkSurface queueFaimilies
  return $ Map.fromList [graphicsQueue, transferFamily, computeFamily, presentationFamily]

getQueuePrioritiesPtr :: Float -> IO (Ptr Float)
getQueuePrioritiesPtr value = 
  alloca $ \ptr -> do
    poke ptr value
    return ptr

getQueueCreateInfo :: Map.Map Word32 a -> Ptr Float -> IO (Map.Map Word32 VkDeviceQueueCreateInfo)
getQueueCreateInfo queueFamilyMap queuePrioritiesPtr = do
  queueCreateInfoList <- forM (Map.keys queueFamilyMap) $ \queueFamilyIndex ->
    newVkData @VkDeviceQueueCreateInfo $ \queueCreateInfoPtr -> do
        writeField @"sType" queueCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        writeField @"pNext" queueCreateInfoPtr VK_NULL_HANDLE
        writeField @"flags" queueCreateInfoPtr 0
        writeField @"queueFamilyIndex" queueCreateInfoPtr queueFamilyIndex
        writeField @"queueCount" queueCreateInfoPtr 1
        writeField @"pQueuePriorities" queueCreateInfoPtr queuePrioritiesPtr
  return $ Map.fromList $ zip (Map.keys queueFamilyMap) queueCreateInfoList

getDeviceCreateInfo :: Ptr VkDeviceQueueCreateInfo
  -> Word32
  -> VkPhysicalDeviceFeatures
  -> [String]
  -> IO VkDeviceCreateInfo
getDeviceCreateInfo queueCreateInfoArrayPtr queueCreateInfoCount physicalDeviceFeatures layers = do
  deviceCreateInfo <- withCStringList layers $ \layerCount layerNames -> do
    newVkData @VkDeviceCreateInfo $ \devCreateInfoPtr -> do
      writeField @"sType" devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
      writeField @"pNext" devCreateInfoPtr VK_NULL_HANDLE
      writeField @"flags" devCreateInfoPtr 0
      writeField @"pQueueCreateInfos" devCreateInfoPtr queueCreateInfoArrayPtr
      writeField @"queueCreateInfoCount" devCreateInfoPtr queueCreateInfoCount
      writeField @"enabledLayerCount" devCreateInfoPtr (fromIntegral layerCount)
      writeField @"ppEnabledLayerNames" devCreateInfoPtr layerNames
      writeField @"enabledExtensionCount" devCreateInfoPtr 0
      writeField @"ppEnabledExtensionNames" devCreateInfoPtr VK_NULL_HANDLE
      writeField @"pEnabledFeatures" devCreateInfoPtr (unsafePtr physicalDeviceFeatures)
  return deviceCreateInfo

createQueues = 1
{-
createQueues :: VkDevice -> Word32 -> IO (Map.Map Word32 VkQueue)
createQueues device queueCreateInfoMap = do
  queueList <- forM (Map.keys queueCreateInfoMap) $ \queueFamilyIndex -> do
    queue <- alloca $ \queuePtr -> do
      vkGetDeviceQueue device queueFamilyIndex 0 queuePtr
      peek queuePtr
  putStrLn $ "Created Graphics Queue: " ++ show (length )
  return queue
-}

createDevice :: VkDeviceCreateInfo -> VkPhysicalDevice -> IO VkDevice
createDevice deviceCreateInfo physicalDevice = do
  device <- alloca $ \devicePtr -> do
    throwingVK "vkCreateDevice: failed to create vkDevice"
      $ vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE devicePtr
    peek devicePtr
  putStrLn $ "Created Device: " ++ show device
  return device

destroyDevice :: VkDevice -> IO ()
destroyDevice device = vkDestroyDevice device VK_NULL_HANDLE


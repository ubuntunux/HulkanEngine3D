{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
    ( getApplicationInfo
    , getInstanceCreateInfo
    , createVulkanInstance
    , destroyVulkanInstance
    , selectPhysicalDevice
    , getQueueFamilyIndex
    , getPhysicalDeviceFeatures
    , getQueueCreateInfo
    , getDeviceCreateInfo
    , createGraphicsDevice
    , destroyDevice
    , touchVKDatas
    ) where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.List ((\\))
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
import Lib.Vulkan


touchVKDatas :: 
  (VulkanMarshal a3, VulkanMarshal a2, VulkanMarshal a1, VulkanMarshal a) 
  => a3 -> a2 -> a1 -> a -> IO ()
touchVKDatas instanceCreateInfo deviceCreateInfo physicalDeviceFeatures queueCreateInfo = do
  touchVkData instanceCreateInfo 
  touchVkData deviceCreateInfo 
  touchVkData physicalDeviceFeatures 
  touchVkData queueCreateInfo

getInstanceExtensionSupport extensions = do
  reqExts <- mapM peekCString extensions
  availExtsC <- asListVK
    $ \x ->
      throwingVK "vkEnumerateInstanceExtensionProperties error"
    . vkEnumerateInstanceExtensionProperties VK_NULL_HANDLE x
  availExts <- mapM ( peekCString
                    . castPtr
                    . ( `plusPtr`
                          fieldOffset @"extensionName" @VkExtensionProperties
                      )
                    . unsafePtr) availExtsC
  putStrLn $ "Available instance extensions : " ++ (show availExts)
  return availExts

getDeviceExtensionSupport pdev extensions = do
  reqExts <- mapM peekCString extensions
  availExtsC <- asListVK
    $ \x ->
      throwingVK "vkEnumerateDeviceExtensionProperties error"
    . vkEnumerateDeviceExtensionProperties pdev VK_NULL_HANDLE x
  availExts <- mapM ( peekCString
                    . castPtr
                    . ( `plusPtr`
                          fieldOffset @"extensionName" @VkExtensionProperties
                      )
                    . unsafePtr) availExtsC
  putStrLn $ "Available device extensions : " ++ (show availExts)
  return availExts

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

selectPhysicalDevice::VkInstance 
  -> Maybe VkSurfaceKHR 
  -> IO (Maybe SwapChainSupportDetails, VkPhysicalDevice)
selectPhysicalDevice vkInstance mVkSurf = do
  devices <- asListVK $ \counterPtr valueArrayPtr ->
    throwingVK "pickPhysicalDevice: Failed to enumerate physical devices." 
      $ vkEnumeratePhysicalDevices vkInstance counterPtr valueArrayPtr
  when (null devices) $ throwVKMsg "Zeo device count!"
  putStrLn $ "Found " ++ show (length devices) ++ " devices."
  selectFirstSuitable devices
  where
    selectFirstSuitable [] = throwVKMsg "No suitable devices!"
    selectFirstSuitable (device:devices) = do
      (maybeSwapChainSupportDetails, result) <- isDeviceSuitable mVkSurf device
      if result then pure (maybeSwapChainSupportDetails, device)
                else selectFirstSuitable devices

getQueueFamilies :: VkPhysicalDevice -> IO [(Word32, VkQueueFamilyProperties)]
getQueueFamilies pdev = alloca $ \qFamCountPtr -> do
  vkGetPhysicalDeviceQueueFamilyProperties pdev qFamCountPtr VK_NULL_HANDLE
  aFamCount <- fromIntegral <$> peek qFamCountPtr
  when (aFamCount <= 0) $ throwVKMsg "Zero queue family count!"
  putStrLn $ "Found " ++ show aFamCount ++ " queue families."

  allocaArray aFamCount $ \familiesPtr -> do
    vkGetPhysicalDeviceQueueFamilyProperties pdev qFamCountPtr familiesPtr
    zip [0..] <$> peekArray aFamCount familiesPtr

selectGraphicsFamily :: [(Word32, VkQueueFamilyProperties)] -> (Word32, VkQueueFamilyProperties)
selectGraphicsFamily [] = throw $ VulkanException Nothing "selectGraphicsFamily: not found!"
selectGraphicsFamily (x@(queueFamilyIndex, queueFamilyProperty):xs) =
  if queueCount > 0 && (queueFlags .&. VK_QUEUE_GRAPHICS_BIT) /= zeroBits
    then x
    else selectGraphicsFamily xs
  where
    queueCount = getField @"queueCount" queueFamilyProperty
    queueFlags = getField @"queueFlags" queueFamilyProperty

    
selectPresentationFamily :: VkPhysicalDevice
     -> VkSurfaceKHR
     -> [(Word32, VkQueueFamilyProperties)]
     -> IO (Word32, VkQueueFamilyProperties)
selectPresentationFamily _ _ [] = throw $ VulkanException Nothing "selectPresentFamily: not Found"
selectPresentationFamily device surface (x@(queueFamilyIndex, queueFamilyProperty):xs)
    | getField @"queueCount" queueFamilyProperty <= 0 = return $ selectGraphicsFamily xs
    | otherwise = do
      supported <- alloca $ \supportedPtr -> do
        throwingVK "vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support."
          $ vkGetPhysicalDeviceSurfaceSupportKHR device queueFamilyIndex surface supportedPtr
        peek supportedPtr
      if VK_TRUE == supported
      then pure x
      else selectPresentationFamily device surface xs


getQueueFamilyIndex :: VkPhysicalDevice -> IO (Word32, VkQueueFamilyProperties)
getQueueFamilyIndex physicalDevice = selectGraphicsFamily <$> getQueueFamilies physicalDevice

getPhysicalDeviceFeatures :: IO VkPhysicalDeviceFeatures
getPhysicalDeviceFeatures = newVkData @VkPhysicalDeviceFeatures clearStorable

getQueueCreateInfo :: Word32 -> IO VkDeviceQueueCreateInfo
getQueueCreateInfo queueFamilyIndex = do
  queueCreateInfo <- alloca $ \queuePrioritiesPtr -> do
    newVkData @VkDeviceQueueCreateInfo $ \queueCreateInfoPtr -> do
        writeField @"sType" queueCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        writeField @"pNext" queueCreateInfoPtr VK_NULL_HANDLE
        writeField @"flags" queueCreateInfoPtr 0
        writeField @"queueFamilyIndex" queueCreateInfoPtr queueFamilyIndex
        writeField @"queueCount" queueCreateInfoPtr 1
        poke queuePrioritiesPtr 1.0
        writeField @"pQueuePriorities" queueCreateInfoPtr queuePrioritiesPtr
  return queueCreateInfo

getDeviceCreateInfo :: VkDeviceQueueCreateInfo
  -> VkPhysicalDeviceFeatures
  -> [String]
  -> IO VkDeviceCreateInfo
getDeviceCreateInfo queueCreateInfo physicalDeviceFeatures layers = do
  deviceCreateInfo <- withCStringList layers $ \layerCount layerNames -> do
    newVkData @VkDeviceCreateInfo $ \devCreateInfoPtr -> do
      writeField @"sType" devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
      writeField @"pNext" devCreateInfoPtr VK_NULL_HANDLE
      writeField @"flags" devCreateInfoPtr 0
      writeField @"pQueueCreateInfos" devCreateInfoPtr (unsafePtr queueCreateInfo)
      writeField @"queueCreateInfoCount" devCreateInfoPtr 1
      writeField @"enabledLayerCount" devCreateInfoPtr (fromIntegral layerCount)
      writeField @"ppEnabledLayerNames" devCreateInfoPtr layerNames
      writeField @"enabledExtensionCount" devCreateInfoPtr 0
      writeField @"ppEnabledExtensionNames" devCreateInfoPtr VK_NULL_HANDLE
      writeField @"pEnabledFeatures" devCreateInfoPtr (unsafePtr physicalDeviceFeatures)
  return deviceCreateInfo

createGraphicsDevice :: VkDeviceCreateInfo -> VkPhysicalDevice -> Word32 -> IO (VkDevice, VkQueue)
createGraphicsDevice deviceCreateInfo physicalDevice queueFamilyIndex = do
  device <- alloca $ \devicePtr -> do
    throwingVK "vkCreateDevice: failed to create vkDevice"
      $ vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE devicePtr
    peek devicePtr    
  queue <- alloca $ \queuePtr -> do
    vkGetDeviceQueue device queueFamilyIndex 0 queuePtr
    peek queuePtr
  putStrLn $ "Created device: " ++ show device
  putStrLn $ "Created queue: " ++ show queue
  return (device, queue)

destroyDevice :: VkDevice -> IO ()
destroyDevice device = vkDestroyDevice device VK_NULL_HANDLE


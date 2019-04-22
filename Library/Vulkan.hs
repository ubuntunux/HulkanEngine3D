{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
    ( createVulkanInstance
    , destroyVulkanInstance
    , selectPhysicalDevice
    , getGraphicsDevice
    , destroyDevice
    ) where

import Control.Exception
import Control.Monad
import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
--import Graphics.Vulkan.Ext.VK_KHR_surface
--import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Lib.Utils
import Lib.Vulkan

      
createVulkanInstance::String -> String -> [CString] -> [String] -> IO VkInstance
createVulkanInstance progName engineName extensions layers = do
  vkInstance <- alloca createAction
  touchVkData iCreateInfo
  return vkInstance
  
  where
    createAction vkInstPtr = do
      throwingVK "vkCreateInstance: Failed to create vkInstance."
        $ vkCreateInstance (unsafePtr iCreateInfo) VK_NULL vkInstPtr    
      peek vkInstPtr

    appInfo = createVk @VkApplicationInfo
      $ set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      &* set @"pNext" VK_NULL
      &* setStrRef @"pApplicationName" progName
      &* set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
      &* setStrRef @"pEngineName" engineName
      &* set @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
      &* set @"apiVersion" (_VK_MAKE_VERSION 1 0 68)
      
    iCreateInfo = createVk @VkInstanceCreateInfo
      $ set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* setVkRef @"pApplicationInfo" appInfo
      &* set @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef @"ppEnabledExtensionNames" extensions


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
    selectFirstSuitable (x:xs) = do
      (mscsd, indeed) <- isDeviceSuitable mVkSurf x
      if indeed then pure (mscsd, x)
                else selectFirstSuitable xs

getQueueFamilies :: VkPhysicalDevice -> IO [(Word32, VkQueueFamilyProperties)]
getQueueFamilies pdev = alloca $ \qFamCountPtr -> do
  vkGetPhysicalDeviceQueueFamilyProperties pdev qFamCountPtr VK_NULL_HANDLE
  aFamCount <- fromIntegral <$> peek qFamCountPtr
  when (aFamCount <= 0) $ throwVKMsg "Zero queue family count!"
  putStrLn $ "Found " ++ show aFamCount ++ " queue families."

  allocaArray aFamCount $ \familiesPtr -> do
    vkGetPhysicalDeviceQueueFamilyProperties pdev qFamCountPtr familiesPtr
    zip [0..] <$> peekArray aFamCount familiesPtr


-- | Throw an error otherwise
selectGraphicsFamily :: [(Word32, VkQueueFamilyProperties)]
                     -> (Word32, VkQueueFamilyProperties)
selectGraphicsFamily []
  = throw $ VulkanException Nothing "selectGraphicsFamily: not found!"
selectGraphicsFamily (x@(_,qfp):xs)
  = if  getField @"queueCount" qfp > 0
     && getField @"queueFlags" qfp .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits
    then x
    else selectGraphicsFamily xs

getGraphicsDevice::VkPhysicalDevice -> IO (VkDevice, VkQueue)
getGraphicsDevice physical_device = do  
  result@(device, queue) <- alloca $ \queuePrioritiesPtr -> do     
    let layers = ["VK_LAYER_LUNARG_standard_validation"]
    withCStringList layers $ \layerCount layerNames -> do

      -- find an appropriate queue family
      (gFamIdx, _gFam) <- selectGraphicsFamily <$> getQueueFamilies physical_device

      qcInfo <- newVkData @VkDeviceQueueCreateInfo
                          $ \qcInfoPtr -> do
        writeField @"sType" qcInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        writeField @"pNext" qcInfoPtr VK_NULL_HANDLE
        writeField @"flags" qcInfoPtr 0
        writeField @"queueFamilyIndex" qcInfoPtr gFamIdx
        writeField @"queueCount" qcInfoPtr 1
        poke queuePrioritiesPtr 1.0
        writeField @"pQueuePriorities" qcInfoPtr queuePrioritiesPtr
      pdevFeatures <- newVkData @VkPhysicalDeviceFeatures clearStorable

      devCreateInfo <- newVkData @VkDeviceCreateInfo
                              $ \devCreateInfoPtr -> do
        writeField @"sType" devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
        writeField @"pNext" devCreateInfoPtr VK_NULL_HANDLE
        writeField @"flags" devCreateInfoPtr 0
        writeField @"pQueueCreateInfos" devCreateInfoPtr (unsafePtr qcInfo)
        writeField @"queueCreateInfoCount" devCreateInfoPtr 1
        writeField @"enabledLayerCount" devCreateInfoPtr (fromIntegral layerCount)
        writeField @"ppEnabledLayerNames" devCreateInfoPtr layerNames
        writeField @"enabledExtensionCount" devCreateInfoPtr 0
        writeField @"ppEnabledExtensionNames" devCreateInfoPtr VK_NULL_HANDLE
        writeField @"pEnabledFeatures" devCreateInfoPtr (unsafePtr pdevFeatures)

      -- try to create a device
      device <- alloca $ \devicePtr -> do
        throwingVK "vkCreateDevice: failed to create vkDevice"
          $ vkCreateDevice physical_device (unsafePtr devCreateInfo) VK_NULL_HANDLE devicePtr
        peek devicePtr

      -- get the first and the only requested queue
      queue <- alloca $ \queuePtr -> do
        vkGetDeviceQueue device gFamIdx 0 queuePtr
        peek queuePtr
      return (device, queue)
  return result

destroyDevice :: VkDevice -> IO ()
destroyDevice device = vkDestroyDevice device VK_NULL_HANDLE
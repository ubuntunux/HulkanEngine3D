{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan
    ( createVulkanInstance
    , destroyVulkanInstance
    , selectPhysicalDevice
    ) where

import Control.Monad
import Foreign.Marshal.Alloc
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

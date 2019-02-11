{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String

main :: IO ()
main = do
    res <- withVulkanInstance $ \vkInstance -> do
      putStrLn $ "Success! " ++ show vkInstance
      pure VK_SUCCESS
    print res


withVulkanInstance::(VkInstance -> IO VkResult) -> IO VkResult
withVulkanInstance action = 
  withCString "01-CreateInstance" $ \programNamePtr ->
  withCString "My Perfect Haskell Engine" $ \engineNamePtr -> do
    appInfo <- newVkData $ \appInfoPtr -> do
      writeField @"engineVersion" appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeField @"pEngineName" appInfoPtr engineNamePtr
      writeField @"applicationVersion" appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeField @"pApplicationName" appInfoPtr programNamePtr
      writeField @"pNext" appInfoPtr VK_NULL_HANDLE
      writeField @"sType" appInfoPtr VK_STRUCTURE_TYPE_APPLICATION_INFO
      writeField @"apiVersion" appInfoPtr (_VK_MAKE_VERSION 1 0 68)

    iCreateInfo <- newVkData $ \iCreateInfoPtr -> do
      writeField @"pApplicationInfo" iCreateInfoPtr (unsafePtr appInfo)
      writeField @"ppEnabledExtensionNames" iCreateInfoPtr VK_NULL_HANDLE
      writeField @"enabledExtensionCount" iCreateInfoPtr 0
      writeField @"ppEnabledLayerNames" iCreateInfoPtr VK_NULL_HANDLE
      writeField @"enabledLayerCount" iCreateInfoPtr 0
      writeField @"flags" iCreateInfoPtr 0
      writeField @"pNext" iCreateInfoPtr VK_NULL_HANDLE
      writeField @"sType" iCreateInfoPtr VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
    
    (vkResult, vkInstance) <- alloca $ \vkInstPtr -> do
      createInst <- vkGetInstanceProc @VkCreateInstance VK_NULL
      vkRes <- createInst (unsafePtr iCreateInfo) VK_NULL_HANDLE vkInstPtr
      vkI <- peek vkInstPtr
      return (vkRes, vkI)
    
    if vkResult == VK_SUCCESS then do
      r <- action vkInstance
      vkDestroyInstance vkInstance VK_NULL_HANDLE
      pure r
    else
      pure vkResult
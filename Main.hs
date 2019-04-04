{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
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

glfwMainLoop :: GLFW.Window -> IO () -> IO ()
glfwMainLoop window mainLoop = go
  where
    go = do
      should <- GLFW.windowShouldClose window
      unless should $ GLFW.pollEvents >> mainLoop >> go

mainLoop::IO()
mainLoop = do
  return ()

main::IO()
main = do 
  maybeWindow <- withGLFWWindow
  when (Nothing == maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
  putStrLn "Initialized GLFW window."
  glfwReqExts <- GLFW.getRequiredInstanceExtensions
  let
    Just window = maybeWindow
    progName = "02-GLFWWindow"
    engineName = "My perfect Haskell engine"
    extensions = glfwReqExts
    layers = ["VK_LAYER_LUNARG_standard_validation"]
  vkInstance <- createVulkanInstance progName engineName extensions layers
  glfwMainLoop window mainLoop
  destroyVulkanInstance vkInstance >> putStrLn "Destroy VulkanInstance."
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()

withGLFWWindow::IO (Maybe GLFW.Window)
withGLFWWindow = do
  GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")  
  putStrLn "Initialized GLFW."
  version <- GLFW.getVersionString
  mapM_ (putStrLn . ("GLFW Version: " ++)) version
  GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable True
  window <- GLFW.createWindow 800 600 "Vulkan Window" Nothing Nothing
  return window
        
    


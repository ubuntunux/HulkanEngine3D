{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Application
    ( glfwMainLoop
    , createGLFWWindow
    , createVkSurface 
    , destroySurface
    ) where


import Control.Exception
import Control.Monad
--import Data.Bits
import Foreign.Marshal.Alloc
--import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.Vulkan
--import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
--import Graphics.Vulkan.Ext.VK_KHR_swapchain
--import Graphics.Vulkan.Marshal.Create
import Lib.Utils
import Lib.Vulkan

import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW


glfwMainLoop :: GLFW.Window -> IO () -> IO ()
glfwMainLoop window mainLoop = go
  where
    go = do
      should <- GLFW.windowShouldClose window
      unless should $ GLFW.pollEvents >> mainLoop >> go


createGLFWWindow::Int -> Int -> String -> IO (Maybe GLFW.Window)
createGLFWWindow width height title = do
  GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")  
  putStrLn "Initialized GLFW."
  version <- GLFW.getVersionString
  mapM_ (putStrLn . ("GLFW Version: " ++)) version
  GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable True
  window <- GLFW.createWindow width height title Nothing Nothing
  return window

createVkSurface :: Storable b => Ptr vkInstance -> GLFW.Window -> IO b
createVkSurface vkInstance window = do
  vkSurface <- alloca $ \vkSurfacePtr -> do
    throwingVK "glfwCreateWindowSurface: failed to create window surface"
      $ GLFW.createWindowSurface vkInstance window VK_NULL_HANDLE vkSurfacePtr
    peek vkSurfacePtr
  return vkSurface

destroySurface :: VkInstance -> VkSurfaceKHR -> IO ()
destroySurface vkInstance vkSurface = do
  destroySurfaceFunc <- vkGetInstanceProc @VkDestroySurfaceKHR vkInstance
  destroySurfaceFunc vkInstance vkSurface VK_NULL_HANDLE
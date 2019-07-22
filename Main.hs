{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import qualified Graphics.UI.GLFW as GLFW
import Library.Utils
import Library.Application
import Library.Vulkan
import qualified Library.Constants as Constants


main::IO()
main = do   
  windowSizeChanged <- newIORef False
  maybeWindow <- createGLFWWindow 800 600 "Vulkan Application" windowSizeChanged
  when (Nothing == maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
  putStrLn "Initialized GLFW window."  
  requireExtensions <- GLFW.getRequiredInstanceExtensions
  instanceExtensionNames <- getInstanceExtensionSupport  
  checkExtensionResult <- checkExtensionSupport instanceExtensionNames requireExtensions
  unless checkExtensionResult (throwVKMsg "Failed to initialize GLFW window.")  
  let
    Just window = maybeWindow
    progName = "Hulkan App"
    engineName = "HulkanEngine3D" 
    isConcurrentMode = True
  defaultRenderData <- getDefaultRenderData
  (renderData', swapChainSupportDetails) <- createRenderer defaultRenderData window progName engineName isConcurrentMode requireExtensions
  renderData <- createRenderData renderData' swapChainSupportDetails
  renderDataRef <- newIORef renderData
  frameIndexRef <- newIORef 0
  imageIndexPtr <- new (0::Word32)
          
  -- Main Loop
  glfwMainLoop window $ do    
    renderData <- readIORef renderDataRef
    frameIndex <- readIORef frameIndexRef
    result <- drawFrame renderData frameIndex imageIndexPtr
    writeIORef frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
    sizeChanged <- readIORef windowSizeChanged
    when (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged) 
      $ do
        atomicWriteIORef windowSizeChanged False
        throwingVK "vkDeviceWaitIdle failed!"
          $ vkDeviceWaitIdle (_device renderData)
        destroyRenderData renderData
        (renderData', swapChainSupportDetails) <- createRenderer defaultRenderData window progName engineName isConcurrentMode requireExtensions
        renderData <- createRenderData renderData' swapChainSupportDetails
        writeIORef renderDataRef renderData
    return True
    
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle (_device renderData)

  -- Terminate
  putStrLn "\n[ Terminate ]"
  renderData <- readIORef renderDataRef
  destroyRenderer renderData  
  free imageIndexPtr
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


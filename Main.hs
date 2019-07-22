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
    
  needCreateRenderDataRef <- newIORef True
  defaultRenderData <- getDefaultRenderData
  renderDataRef <- newIORef defaultRenderData
  frameIndexRef <- newIORef 0
  imageIndexPtr <- new (0::Word32)
          
  -- Main Loop
  glfwMainLoop window $ do    
    needCreateRenderData <- readIORef needCreateRenderDataRef
    when needCreateRenderData $ do
      writeIORef needCreateRenderDataRef False
      readRenderData <- readIORef renderDataRef
      when (VK_NULL /= _device readRenderData) $ do        
        throwingVK "vkDeviceWaitIdle failed!"
          $ vkDeviceWaitIdle (_device readRenderData)
        destroyRenderData readRenderData
      (preRenderData, swapChainSupportDetails) <- createRenderer defaultRenderData window progName engineName isConcurrentMode requireExtensions
      newRenderData <- createRenderData preRenderData swapChainSupportDetails
      writeIORef renderDataRef newRenderData
    renderData <- readIORef renderDataRef
    frameIndex <- readIORef frameIndexRef
    result <- drawFrame renderData frameIndex imageIndexPtr
    writeIORef frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
    sizeChanged <- readIORef windowSizeChanged
    when (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged) 
      $ do
        atomicWriteIORef windowSizeChanged False
        writeIORef needCreateRenderDataRef True
    return True

  -- Terminate
  putStrLn "\n[ Terminate ]"  
  renderData <- readIORef renderDataRef
  throwingVK "vkDeviceWaitIdle failed!"
    $ vkDeviceWaitIdle (_device renderData)
  destroyRenderer renderData  
  free imageIndexPtr
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


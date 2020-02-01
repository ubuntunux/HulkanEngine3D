{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.IORef
import Data.Maybe (isNothing)
import qualified Data.DList as DList
import System.CPUTime
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import qualified Graphics.UI.GLFW as GLFW

import Library.Utils
import Library.Application
import Library.Logger
import Library.Vulkan
import Library.Vulkan.Device
import Library.Vulkan.Mesh
import Library.Vulkan.RenderPass
import Library.Resource.ObjLoader
import qualified Library.Constants as Constants


main::IO()
main = do
    windowSizeChanged <- newIORef False
    maybeWindow <- createGLFWWindow 1024 768 "Vulkan Application" windowSizeChanged
    when (isNothing maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
    logInfo "<< Initialized GLFW window >>"
    requireExtensions <- GLFW.getRequiredInstanceExtensions
    instanceExtensionNames <- getInstanceExtensionSupport
    checkExtensionResult <- checkExtensionSupport instanceExtensionNames requireExtensions
    unless checkExtensionResult (throwVKMsg "Failed to initialize GLFW window.")
    let Just window = maybeWindow
        progName = "Hulkan App"
        engineName = "HulkanEngine3D"
        isConcurrentMode = True
    -- create renderer
    defaultRendererData <- getDefaultRendererData
    rendererData <- createRenderer defaultRendererData window progName engineName isConcurrentMode requireExtensions
    rendererDataRef <- newIORef rendererData

    -- create render pass data
    commandBuffers <- getCommandBuffers rendererData
    renderPassCreateInfo <- getDefaultRenderPassCreateInfo rendererData
    renderPassData <- createRenderPassData (_device rendererData) commandBuffers renderPassCreateInfo
    renderPassDataListRef <- newIORef (DList.singleton renderPassData)

    -- create resources
    (vertices, indices) <- loadModel "Resource/Externals/Meshes/suzan.obj"
    geometryBuffer <- createGeometryBuffer "test" rendererData vertices indices
    geometryBufferListRef <- newIORef (DList.singleton geometryBuffer)

    needRecreateSwapChainRef <- newIORef False
    frameIndexRef <- newIORef 0
    imageIndexPtr <- new (0 :: Word32)
    currentTime <- getCPUTime
    currentTimeRef <- newIORef currentTime
    elapsedTimeRef <- newIORef (0.0 :: Double)
    --resourceRef <- newIORef Resource
    -- Main Loop
    glfwMainLoop window $ do
        currentTime <- getCPUTime
        previousTime <- readIORef currentTimeRef
        let deltaTime = fromIntegral (currentTime - previousTime) / Constants.convertToSecond
        elapsedTime <- do
            elapsedTimePrev <- readIORef elapsedTimeRef
            return $ elapsedTimePrev + deltaTime
        writeIORef currentTimeRef currentTime
        writeIORef elapsedTimeRef elapsedTime
        --when (0.0 < deltaTime) $ print (1.0 / deltaTime)

        needRecreateSwapChain <- readIORef needRecreateSwapChainRef
        when needRecreateSwapChain $ do
            writeIORef needRecreateSwapChainRef False
            logInfo "<< Recreate SwapChain >>"

            -- cleanUp swapChain
            rendererData <- readIORef rendererDataRef
            result <- vkDeviceWaitIdle $ _device rendererData
            validationVK result "vkDeviceWaitIdle failed!"

            renderPassDataList <- readIORef renderPassDataListRef
            forM_ renderPassDataList $ \renderPassData -> do
                destroyRenderPassData (_device rendererData) renderPassData

            -- recreate swapChain
            rendererData <- recreateSwapChain rendererData window
            commandBuffers <- getCommandBuffers rendererData
            renderPassCreateInfo <- getDefaultRenderPassCreateInfo rendererData
            renderPassData <- createRenderPassData (_device rendererData) commandBuffers renderPassCreateInfo
            writeIORef renderPassDataListRef (DList.fromList [renderPassData])
            writeIORef rendererDataRef rendererData
        frameIndex <- readIORef frameIndexRef
        rendererData <- readIORef rendererDataRef
        renderPassDataList <- readIORef renderPassDataListRef
        result <- drawFrame rendererData frameIndex imageIndexPtr
        writeIORef frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount
        sizeChanged <- readIORef windowSizeChanged
        when (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result || sizeChanged) $ do
            atomicWriteIORef windowSizeChanged False
            writeIORef needRecreateSwapChainRef True
        return True
    -- Terminate
    logInfo "<< Terminate >>"
    rendererData <- readIORef rendererDataRef
    result <- vkDeviceWaitIdle $ _device rendererData
    validationVK result "vkDeviceWaitIdle failed!"

    geometryBufferList <- readIORef geometryBufferListRef
    forM_ geometryBufferList $ \geometryBuffer -> do
        destroyGeometryBuffer (_device rendererData) geometryBuffer

    renderPassDataList <- readIORef renderPassDataListRef
    forM_ renderPassDataList $ \renderPassData -> do
        destroyRenderPassData (_device rendererData) renderPassData

    destroyRenderer rendererData
    free imageIndexPtr
    return ()

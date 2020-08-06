{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE InstanceSigs               #-}

module HulkanEngine3D.Render.Renderer
  ( RendererData (..)
  , RendererInterface (..)
  , createRenderer
  , destroyRenderer
  , initializeRenderer
  , resizeWindow
  , recreateSwapChain
  , renderScene
  ) where

import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.DList as DList
import qualified Data.Text as Text
import qualified Data.HashTable.IO as HashTable
import Data.IORef
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Numeric.DataFrame

import qualified HulkanEngine3D.Constants as Constants
import {-# SOURCE #-} qualified HulkanEngine3D.Application.SceneManager as SceneManager
import qualified HulkanEngine3D.Render.Camera as Camera
import qualified HulkanEngine3D.Render.Light as Light
import {-# SOURCE #-} HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Render.RenderTargetDeclaration
import HulkanEngine3D.Render.ImageSampler
import HulkanEngine3D.Render.MaterialInstance
import qualified HulkanEngine3D.Render.RenderElement as RenderElement
import qualified HulkanEngine3D.Render.RenderObject as RenderObject
import qualified HulkanEngine3D.Render.TransformObject as TransformObject
import qualified HulkanEngine3D.Render.Mesh as Mesh
import qualified HulkanEngine3D.Render.PostProcess as PostProcess
import HulkanEngine3D.Render.UniformBufferDatas
import {-# SOURCE #-} HulkanEngine3D.Resource.Resource
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.Buffer
import HulkanEngine3D.Vulkan.CommandBuffer
import HulkanEngine3D.Vulkan.Device
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.GeometryBuffer
import HulkanEngine3D.Vulkan.Queue
import HulkanEngine3D.Vulkan.PushConstant
import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import HulkanEngine3D.Vulkan.SwapChain
import HulkanEngine3D.Vulkan.Sync
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import HulkanEngine3D.Vulkan.UniformBuffer


data RendererData = RendererData
    { _frameIndexRef :: IORef Int
    , _swapChainIndexPtr :: Ptr Word32
    , _vertexOffsetPtr :: Ptr VkDeviceSize
    , _needRecreateSwapChainRef :: IORef Bool
    , _isFirstRender :: IORef Bool
    , _imageAvailableSemaphores :: FrameIndexMap VkSemaphore
    , _renderFinishedSemaphores :: FrameIndexMap VkSemaphore
    , _vkInstance :: VkInstance
    , _vkSurface :: VkSurfaceKHR
    , _device :: VkDevice
    , _physicalDevice :: VkPhysicalDevice
    , _swapChainDataRef :: IORef SwapChainData
    , _swapChainSupportDetailsRef :: IORef SwapChainSupportDetails
    , _queueFamilyDatas :: QueueFamilyDatas
    , _frameFencesPtr :: Ptr VkFence
    , _commandPool :: VkCommandPool
    , _commandBufferCount :: Int
    , _commandBuffersPtr :: Ptr VkCommandBuffer
    , _renderFeatures :: RenderFeatures
    , _imageSamplers :: IORef ImageSamplers
    , _debugRenderTargetRef :: IORef RenderTargetType
    , _renderTargetDataMap :: RenderTargetDataMap
    , _uniformBufferDataMap :: UniformBufferDataMap
    , _postprocess_ssao :: PostProcess.PostProcessData
    , _resources :: Resources
    } deriving (Show)


class RendererInterface a where
    getPhysicalDevice :: a -> VkPhysicalDevice
    getDevice :: a -> VkDevice
    getSwapChainData :: a -> IO SwapChainData
    getSwapChainImageViews :: a -> IO (SwapChainIndexMap VkImageView)
    getSwapChainSupportDetails :: a -> IO SwapChainSupportDetails
    getSwapChainIndex :: a -> IO Int
    getCommandPool :: a -> VkCommandPool
    getCommandBuffers :: a -> IO [VkCommandBuffer]
    getCommandBuffer :: a -> Int -> IO VkCommandBuffer
    getCurrentCommandBuffer :: a -> IO VkCommandBuffer
    getGraphicsQueue :: a -> VkQueue
    getPresentQueue :: a -> VkQueue
    getUniformBufferData :: a -> UniformBufferType -> IO UniformBufferData
    nextDebugRenderTarget :: a -> IO ()
    prevDebugRenderTarget :: a -> IO ()
    createRenderTarget :: a -> Text.Text -> Texture.TextureCreateInfo -> IO Texture.TextureData
    createTexture :: a -> Text.Text -> Texture.TextureCreateInfo -> IO Texture.TextureData
    destroyTexture :: a -> Texture.TextureData -> IO ()
    getRenderTarget :: a -> RenderTargetType -> IO Texture.TextureData
    createGeometryBuffer :: a -> Text.Text -> GeometryCreateInfo -> IO GeometryData
    destroyGeometryBuffer :: a -> GeometryData -> IO ()
    beginRenderPassPipeline :: a -> VkCommandBuffer -> Int -> MaterialInstanceData -> IO ()
    drawElements :: a -> VkCommandBuffer -> GeometryData -> IO ()
    deviceWaitIdle :: a -> IO ()

instance RendererInterface RendererData where
    getPhysicalDevice rendererData = (_physicalDevice rendererData)
    getDevice rendererData = (_device rendererData)
    getSwapChainData rendererData = readIORef $ _swapChainDataRef rendererData
    getSwapChainImageViews rendererData = _swapChainImageViews <$> getSwapChainData rendererData
    getSwapChainSupportDetails rendererData = readIORef $ _swapChainSupportDetailsRef rendererData
    getSwapChainIndex renderData = fromIntegral <$> peek (_swapChainIndexPtr renderData)
    getCommandPool rendererData = (_commandPool rendererData)
    getCommandBuffers rendererData = peekArray (_commandBufferCount rendererData) (_commandBuffersPtr rendererData)
    getCommandBuffer rendererData index = peekElemOff (_commandBuffersPtr rendererData) index
    getCurrentCommandBuffer rendererData = getCommandBuffer rendererData =<< getSwapChainIndex rendererData
    getGraphicsQueue rendererData = (_graphicsQueue (_queueFamilyDatas rendererData))
    getPresentQueue rendererData = (_presentQueue (_queueFamilyDatas rendererData))

    getUniformBufferData :: RendererData -> UniformBufferType -> IO UniformBufferData
    getUniformBufferData rendererData uniformBufferType =
        Maybe.fromJust <$> HashTable.lookup (_uniformBufferDataMap rendererData) uniformBufferType

    nextDebugRenderTarget rendererData = do
        debugRenderTarget <- readIORef (_debugRenderTargetRef rendererData)
        let nextValue = if debugRenderTarget == (maxBound::RenderTargetType) then (minBound::RenderTargetType) else succ debugRenderTarget
        logInfo $ "Current DebugRenderTarget : " ++ show nextValue
        writeIORef (_debugRenderTargetRef rendererData) nextValue

    prevDebugRenderTarget rendererData = do
        debugRenderTarget <- readIORef (_debugRenderTargetRef rendererData)
        let prevValue = if debugRenderTarget == (minBound::RenderTargetType) then (maxBound::RenderTargetType) else pred debugRenderTarget
        logInfo $ "Current DebugRenderTarget : " ++ show prevValue
        writeIORef (_debugRenderTargetRef rendererData) prevValue

    createRenderTarget rendererData textureDataName textureCreateInfo =
        Texture.createRenderTarget
            textureDataName
            (_physicalDevice rendererData)
            (_device rendererData)
            (_commandPool rendererData)
            (_graphicsQueue $ _queueFamilyDatas rendererData)
            textureCreateInfo

    createTexture rendererData textureDataName textureCreateInfo =
        Texture.createTextureData
            textureDataName
            (_physicalDevice rendererData)
            (_device rendererData)
            (_commandPool rendererData)
            (_graphicsQueue $ _queueFamilyDatas rendererData)
            textureCreateInfo

    destroyTexture rendererData textureData =
        Texture.destroyTextureData (_device rendererData) textureData

    getRenderTarget rendererData renderTargetType =
        Maybe.fromJust <$> HashTable.lookup (_renderTargetDataMap rendererData) renderTargetType

    createGeometryBuffer rendererData bufferName geometryCreateInfo = do
        createGeometryData
            (getPhysicalDevice rendererData)
            (getDevice rendererData)
            (getGraphicsQueue rendererData)
            (getCommandPool rendererData)
            bufferName
            geometryCreateInfo

    destroyGeometryBuffer rendererData geometryBuffer =
        destroyGeometryData (_device rendererData) geometryBuffer

    beginRenderPassPipeline rendererData commandBuffer swapChainIndex materialInstanceData =
        beginRenderPassPipeline' commandBuffer swapChainIndex (_resources rendererData) materialInstanceData

    drawElements rendererData commandBuffer geometryData =
        drawElements' commandBuffer geometryData (_vertexOffsetPtr rendererData)

    deviceWaitIdle rendererData =
        throwingVK "vkDeviceWaitIdle failed!" (vkDeviceWaitIdle $ getDevice rendererData)

defaultRendererData :: Resources -> IO RendererData
defaultRendererData resources = do
    imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
        writeField @"width" extentPtr $ 0
        writeField @"height" extentPtr $ 0
    surfaceCapabilities <- newVkData @VkSurfaceCapabilitiesKHR $ \surfaceCapabilitiesPtr -> do
        return ()
    let defaultSwapChainData = SwapChainData
            { _swapChain = VK_NULL
            , _swapChainImages = SwapChainIndexMapEmpty
            , _swapChainImageFormat = VK_FORMAT_UNDEFINED
            , _swapChainImageViews = SwapChainIndexMapEmpty
            , _swapChainExtent = imageExtent }
        defaultSwapChainSupportDetails = SwapChainSupportDetails
            { _capabilities = surfaceCapabilities
            , _formats = []
            , _presentModes = [] }
        defaultQueueFamilyIndices = QueueFamilyIndices
            { _graphicsQueueIndex = 0
            , _presentQueueIndex = 0
            , _computeQueueIndex = 0
            , _transferQueueIndex = 0
            , _sparseBindingQueueIndex = 0 }
        defaultQueueFamilyDatas = QueueFamilyDatas
            { _graphicsQueue = VK_NULL
            , _presentQueue = VK_NULL
            , _queueFamilyIndexList = []
            , _queueFamilyCount = 0
            , _queueFamilyIndices = defaultQueueFamilyIndices }
        defaultRenderFeatures = RenderFeatures
            { _anisotropyEnable = VK_FALSE
            , _msaaSamples = VK_SAMPLE_COUNT_1_BIT }
    postprocess_ssao <- PostProcess.initializePostProcessData_SSAO
    swapChainIndexPtr <- new (0 :: Word32)
    vertexOffsetPtr <- new (0 :: VkDeviceSize)
    frameIndexRef <- newIORef (0::Int)
    needRecreateSwapChainRef <- newIORef False
    isFirstRender <- newIORef True
    imageSamplers <- newIORef defaultImageSamplers
    renderPassDataListRef <- newIORef (DList.fromList [])
    swapChainDataRef <- newIORef defaultSwapChainData
    swapChainSupportDetailsRef <- newIORef defaultSwapChainSupportDetails
    debugRenderTargetRef <- newIORef RenderTarget_BackBuffer
    renderTargetDataMap <- HashTable.new
    uniformBufferDataMap <- HashTable.new

    return RendererData
        { _frameIndexRef = frameIndexRef
        , _swapChainIndexPtr = swapChainIndexPtr
        , _vertexOffsetPtr = vertexOffsetPtr
        , _needRecreateSwapChainRef = needRecreateSwapChainRef
        , _isFirstRender = isFirstRender
        , _imageAvailableSemaphores = FrameIndexMapEmpty
        , _renderFinishedSemaphores = FrameIndexMapEmpty
        , _vkInstance = VK_NULL
        , _vkSurface = VK_NULL
        , _device = VK_NULL
        , _physicalDevice = VK_NULL
        , _swapChainDataRef = swapChainDataRef
        , _swapChainSupportDetailsRef = swapChainSupportDetailsRef
        , _queueFamilyDatas = defaultQueueFamilyDatas
        , _frameFencesPtr = VK_NULL
        , _commandPool = VK_NULL
        , _commandBufferCount = 0
        , _commandBuffersPtr = VK_NULL
        , _renderFeatures = defaultRenderFeatures
        , _imageSamplers = imageSamplers
        , _debugRenderTargetRef = debugRenderTargetRef
        , _renderTargetDataMap = renderTargetDataMap
        , _uniformBufferDataMap = uniformBufferDataMap
        , _postprocess_ssao = postprocess_ssao
        , _resources = resources
        }

initializeRenderer :: RendererData -> IO RendererData
initializeRenderer rendererData@RendererData {..} = do
    poke _swapChainIndexPtr 0
    writeIORef _frameIndexRef (0::Int)
    writeIORef _needRecreateSwapChainRef False

    registUniformBufferDatas _physicalDevice _device _uniformBufferDataMap

    imageSamplers <- createImageSamplers _device
    writeIORef _imageSamplers imageSamplers

    renderTargets <- createRenderTargets rendererData _renderTargetDataMap

    return rendererData

createRenderer :: GLFW.Window
               -> String
               -> String
               -> Bool
               -> Bool
               -> [CString]
               -> Resources
               -> IO RendererData
createRenderer window progName engineName enableValidationLayer isConcurrentMode requireExtensions resources = do
    let validationLayers = if enableValidationLayer then Constants.vulkanLayers else []
    if enableValidationLayer
    then logInfo $ "Enable validation layers : " ++ show validationLayers
    else logInfo $ "Disabled validation layers"

    defaultRendererData <- defaultRendererData resources

    vkInstance <- createVulkanInstance progName engineName validationLayers requireExtensions
    vkSurface <- createVkSurface vkInstance window
    (physicalDevice, Just swapChainSupportDetails, supportedFeatures) <-
        selectPhysicalDevice vkInstance (Just vkSurface)
    deviceProperties <- getPhysicalDeviceProperties physicalDevice
    msaaSamples <- getMaxUsableSampleCount deviceProperties
    queueFamilyIndices <- getQueueFamilyIndices physicalDevice vkSurface isConcurrentMode
    let renderFeatures = RenderFeatures
            { _anisotropyEnable = getField @"samplerAnisotropy" supportedFeatures
            , _msaaSamples = msaaSamples }
    let graphicsQueueIndex = _graphicsQueueIndex queueFamilyIndices
        presentQueueIndex = _presentQueueIndex queueFamilyIndices
        queueFamilyIndexList = Set.toList $ Set.fromList [graphicsQueueIndex, presentQueueIndex]
    device <- createDevice physicalDevice queueFamilyIndexList
    queueMap <- createQueues device queueFamilyIndexList
    let defaultQueue = (Map.elems queueMap) !! 0
        queueFamilyDatas = QueueFamilyDatas
            { _graphicsQueue = Maybe.fromMaybe defaultQueue $ Map.lookup graphicsQueueIndex queueMap
            , _presentQueue = Maybe.fromMaybe defaultQueue $ Map.lookup presentQueueIndex queueMap
            , _queueFamilyIndexList = queueFamilyIndexList
            , _queueFamilyCount = fromIntegral $ length queueMap
            , _queueFamilyIndices = queueFamilyIndices }
    commandPool <- createCommandPool device queueFamilyDatas
    imageAvailableSemaphores <- createSemaphores device
    renderFinishedSemaphores <- createSemaphores device
    frameFencesPtr <- createFrameFences device

    swapChainData <- createSwapChainData device swapChainSupportDetails queueFamilyDatas vkSurface Constants.enableImmediateMode
    swapChainDataRef <- newIORef swapChainData
    swapChainSupportDetailsRef <- newIORef swapChainSupportDetails

    let commandBufferCount = Constants.swapChainImageCount
    commandBuffersPtr <- mallocArray commandBufferCount::IO (Ptr VkCommandBuffer)
    createCommandBuffers device commandPool commandBufferCount commandBuffersPtr
    commandBuffers <- peekArray commandBufferCount commandBuffersPtr

    let rendererData = defaultRendererData
            { _imageAvailableSemaphores = imageAvailableSemaphores
            , _renderFinishedSemaphores = renderFinishedSemaphores
            , _vkInstance = vkInstance
            , _vkSurface = vkSurface
            , _device = device
            , _physicalDevice = physicalDevice
            , _queueFamilyDatas = queueFamilyDatas
            , _frameFencesPtr = frameFencesPtr
            , _commandPool = commandPool
            , _commandBuffersPtr = commandBuffersPtr
            , _commandBufferCount = commandBufferCount
            , _swapChainDataRef = swapChainDataRef
            , _swapChainSupportDetailsRef = swapChainSupportDetailsRef
            , _renderFeatures = renderFeatures
            }

    initializeRenderer rendererData

destroyRenderer :: RendererData -> IO ()
destroyRenderer rendererData@RendererData {..} = do
    destroyUniformBufferDatas _device _uniformBufferDataMap

    imageSamplers <- readIORef _imageSamplers
    destroyImageSamplers _device imageSamplers

    destroyRenderTargets rendererData _renderTargetDataMap

    destroySemaphores _device _renderFinishedSemaphores
    destroySemaphores _device _imageAvailableSemaphores
    destroyFrameFences _device _frameFencesPtr
    destroyCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
    destroyCommandPool _device _commandPool
    swapChainData <- (getSwapChainData rendererData)
    destroySwapChainData _device swapChainData
    destroyDevice _device
    destroyVkSurface _vkInstance _vkSurface
    destroyVulkanInstance _vkInstance
    free _commandBuffersPtr
    free _swapChainIndexPtr
    free _vertexOffsetPtr


resizeWindow :: GLFW.Window -> RendererData -> IO ()
resizeWindow window rendererData@RendererData {..} = do
    logInfo "<< resizeWindow >>"

    deviceWaitIdle rendererData

    -- destroy swapchain & graphics resources
    unloadGraphicsDatas _resources rendererData

    destroyRenderTargets rendererData _renderTargetDataMap

    -- recreate swapchain & graphics resources
    recreateSwapChain rendererData window

    renderTargets <- createRenderTargets rendererData _renderTargetDataMap

    loadGraphicsDatas _resources rendererData


recreateSwapChain :: RendererData -> GLFW.Window -> IO ()
recreateSwapChain rendererData@RendererData {..} window = do
    logInfo "<< recreateSwapChain >>"
    destroyCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr
    swapChainData <- getSwapChainData rendererData
    destroySwapChainData _device swapChainData

    newSwapChainSupportDetails <- querySwapChainSupport _physicalDevice _vkSurface
    newSwapChainData <- createSwapChainData _device newSwapChainSupportDetails _queueFamilyDatas _vkSurface Constants.enableImmediateMode
    writeIORef _swapChainDataRef newSwapChainData
    writeIORef _swapChainSupportDetailsRef newSwapChainSupportDetails

    createCommandBuffers _device _commandPool _commandBufferCount _commandBuffersPtr


presentSwapChain :: RendererData -> Ptr VkCommandBuffer -> Ptr VkFence -> VkSemaphore -> VkSemaphore -> IO VkResult
presentSwapChain rendererData@RendererData {..} commandBufferPtr frameFencePtr imageAvailableSemaphore renderFinishedSemaphore = do
    let QueueFamilyDatas {..} = _queueFamilyDatas
    swapChainData@SwapChainData {..} <- getSwapChainData rendererData

    let submitInfo = createVk @VkSubmitInfo
              $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
              &* set @"pNext" VK_NULL
              &* set @"waitSemaphoreCount" 1
              &* setListRef @"pWaitSemaphores" [imageAvailableSemaphore]
              &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
              &* set @"commandBufferCount" 1
              &* set @"pCommandBuffers" commandBufferPtr
              &* set @"signalSemaphoreCount" 1
              &* setListRef @"pSignalSemaphores" [renderFinishedSemaphore]

    vkResetFences _device 1 frameFencePtr

    frameFence <- peek frameFencePtr

    let waitingForFence = False

    withPtr submitInfo $ \submitInfoPtr ->
        vkQueueSubmit _graphicsQueue 1 submitInfoPtr (if waitingForFence then frameFence else VK_NULL) >>=
          flip validationVK "vkQueueSubmit failed!"

    when waitingForFence $
        vkWaitForFences _device 1 frameFencePtr VK_TRUE (maxBound :: Word64) >>=
            flip validationVK "vkWaitForFences failed!"

    let presentInfo = createVk @VkPresentInfoKHR
          $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
          &* set @"pNext" VK_NULL
          &* set @"pImageIndices" _swapChainIndexPtr
          &* set @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [renderFinishedSemaphore]
          &* set @"swapchainCount" 1
          &* setListRef @"pSwapchains" [_swapChain]

    result <- withPtr presentInfo $ \presentInfoPtr -> do
        vkQueuePresentKHR _presentQueue presentInfoPtr

    -- waiting
    deviceWaitIdle rendererData
    return result


beginRenderPassPipeline' :: VkCommandBuffer
                         -> Int
                         -> Resources
                         -> MaterialInstanceData
                         -> IO ()
beginRenderPassPipeline' commandBuffer swapChainIndex resources materialInstanceData = do
    let renderPassData = _renderPassData materialInstanceData
    Just frameBufferData <- getFrameBufferData resources (RenderPass.getRenderPassFrameBufferName renderPassData)
    let renderPassBeginInfo = atSwapChainIndex swapChainIndex (_renderPassBeginInfos frameBufferData)
        descriptorSetsPtr = _descriptorSetsPtr materialInstanceData
        pipelineData = RenderPass.getDefaultPipelineData renderPassData
        pipelineDynamicStates = RenderPass._pipelineDynamicStates pipelineData

    withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
        vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE

    when (elem VK_DYNAMIC_STATE_VIEWPORT pipelineDynamicStates) $
        withPtr (_frameBufferViewPort . _frameBufferInfo $ frameBufferData) $ \viewPortPtr ->
            vkCmdSetViewport commandBuffer 0 1 viewPortPtr

    when (elem VK_DYNAMIC_STATE_SCISSOR pipelineDynamicStates) $
        withPtr (_frameBufferScissorRect . _frameBufferInfo $ frameBufferData) $ \scissorRectPtr ->
            vkCmdSetScissor commandBuffer 0 1 scissorRectPtr

    vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS (RenderPass._pipeline pipelineData)
    vkCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS (RenderPass._pipelineLayout pipelineData) 0 1 (ptrAtIndex descriptorSetsPtr swapChainIndex) 0 VK_NULL


drawElements' :: VkCommandBuffer -> GeometryData -> Ptr VkDeviceSize -> IO ()
drawElements' commandBuffer geometryData vertexOffsetPtr = do
    vkCmdBindVertexBuffers commandBuffer 0 1 (_vertexBufferPtr geometryData) vertexOffsetPtr
    vkCmdBindIndexBuffer commandBuffer (_indexBuffer geometryData) 0 VK_INDEX_TYPE_UINT32
    vkCmdDrawIndexed commandBuffer (_vertexIndexCount geometryData) 1 0 0 0
    vkCmdEndRenderPass commandBuffer


uploadUniformBufferData :: (Storable a) => RendererData -> Int -> UniformBufferType -> a -> IO ()
uploadUniformBufferData rendererData@RendererData {..} swapChainIndex uniformBufferType uploadData = do
    uniformBufferData <- getUniformBufferData rendererData uniformBufferType
    let uniformBufferMemory = atSwapChainIndex swapChainIndex (_uniformBufferMemories uniformBufferData)
    updateBufferData _device uniformBufferMemory uploadData


renderScene :: RendererData -> SceneManager.SceneManagerData -> Double -> Float -> IO ()
renderScene rendererData@RendererData {..} sceneManagerData elapsedTime deltaTime = do
    -- frame index
    frameIndex <- readIORef _frameIndexRef
    let frameFencePtr = ptrAtIndex _frameFencesPtr frameIndex
        imageAvailableSemaphore = atFrameIndex frameIndex _imageAvailableSemaphores
        renderFinishedSemaphore = atFrameIndex frameIndex _renderFinishedSemaphores
    swapChainData@SwapChainData {..} <- getSwapChainData rendererData

    -- Begin Render
    acquireNextImageResult <- vkAcquireNextImageKHR _device _swapChain maxBound imageAvailableSemaphore VK_NULL_HANDLE _swapChainIndexPtr
    swapChainIndex <- getSwapChainIndex rendererData
    let commandBufferPtr = ptrAtIndex _commandBuffersPtr swapChainIndex
    commandBuffer <- peek commandBufferPtr

    result <- case acquireNextImageResult of
        VK_SUCCESS -> do
            isFirstRender <- readIORef _isFirstRender

            mainCamera <- SceneManager.getMainCamera sceneManagerData
            cameraPosition <- Camera.getCameraPosition mainCamera
            viewMatrix <- Camera.getViewMatrix mainCamera
            invViewMatrix <- Camera.getInvViewMatrix mainCamera
            viewOriginMatrix <- Camera.getViewOriginMatrix mainCamera
            invViewOriginMatrix <- Camera.getInvViewOriginMatrix mainCamera
            projectionMatrix <- Camera.getProjectionMatrix mainCamera
            invProjectionMatrix <- Camera.getInvProjectionMatrix mainCamera
            viewProjectionMatrix <- Camera.getViewProjectionMatrix mainCamera
            invViewProjectionMatrix <- Camera.getInvViewProjectionMatrix mainCamera
            viewOriginProjectionMatrix <- Camera.getViewOriginProjectionMatrix mainCamera
            invViewOriginProjectionMatrix <- Camera.getInvViewOriginProjectionMatrix mainCamera

            mainLight <- SceneManager.getMainLight sceneManagerData
            mainLightConstants <- Light.getLightConstants mainLight
            quadGeometryData <- Mesh.getDefaultGeometryData =<< getMeshData _resources "quad"

            rotation <- TransformObject.getRotation $ Light._directionalLightTransformObject mainLight

            -- Upload Uniform Buffers
            let screenWidth = fromIntegral $ getField @"width" _swapChainExtent :: Float
                screenHeight = fromIntegral $ getField @"height" _swapChainExtent :: Float
                sceneConstants = SceneConstants
                    { _SCREEN_SIZE = vec2 screenWidth screenHeight
                    , _BACKBUFFER_SIZE = vec2 screenWidth screenHeight
                    , _TIME = realToFrac elapsedTime
                    , _DELTA_TIME = scalar deltaTime
                    , _JITTER_FRAME = float_zero
                    , _SceneConstantsDummy0 = 0
                    }
                viewConstants = ViewConstants
                    { _VIEW  = viewMatrix
                    , _INV_VIEW = invViewMatrix
                    , _VIEW_ORIGIN = viewOriginMatrix
                    , _INV_VIEW_ORIGIN = invViewOriginMatrix
                    , _PROJECTION = projectionMatrix
                    , _INV_PROJECTION = invProjectionMatrix
                    , _VIEW_PROJECTION = viewProjectionMatrix
                    , _INV_VIEW_PROJECTION = invViewProjectionMatrix
                    , _VIEW_ORIGIN_PROJECTION = viewOriginProjectionMatrix
                    , _INV_VIEW_ORIGIN_PROJECTION = invViewOriginProjectionMatrix
                    , _NEAR_FAR = vec2 Constants.near Constants.far
                    , _JITTER_DELTA = vec2 0.0 0.0
                    , _JITTER_OFFSET = vec2 0.0 0.0
                    , _VIEWCONSTANTS_DUMMY0 = 0.0
                    , _VIEWCONSTANTS_DUMMY1 = 0.0
                    , _CAMERA_POSITION = cameraPosition
                    , _VIEWCONSTANTS_DUMMY2 = 0.0
                    }
            uploadUniformBufferData rendererData swapChainIndex UniformBuffer_SceneConstants sceneConstants
            uploadUniformBufferData rendererData swapChainIndex UniformBuffer_ViewConstants viewConstants
            uploadUniformBufferData rendererData swapChainIndex UniformBuffer_LightConstants mainLightConstants
            uploadUniformBufferData rendererData swapChainIndex UniformBuffer_SSAOConstants (PostProcess._ssao_kernel_samples _postprocess_ssao)

            -- Begin command buffer
            let commandBufferBeginInfo = createVk @VkCommandBufferBeginInfo
                    $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                    &* set @"pNext" VK_NULL
                    &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
            withPtr commandBufferBeginInfo $ \commandBufferBeginInfoPtr -> do
                result <- vkBeginCommandBuffer commandBuffer commandBufferBeginInfoPtr
                validationVK result "vkBeginCommandBuffer failed!"

            -- Render
            renderShadow rendererData commandBuffer swapChainIndex sceneManagerData
            renderSolid rendererData commandBuffer swapChainIndex sceneManagerData
            renderPostProcess rendererData commandBuffer swapChainIndex quadGeometryData

            -- Render Final
            materialInst_renderFinal <- getMaterialInstanceData _resources "render_final"
            beginRenderPassPipeline rendererData commandBuffer swapChainIndex materialInst_renderFinal
            drawElements rendererData commandBuffer quadGeometryData

            -- Render Debug
            --writeIORef _debugRenderTargetRef RenderTarget_Shadow
            debugRenderTarget <- readIORef _debugRenderTargetRef
            when (RenderTarget_BackBuffer /= debugRenderTarget) $ do
                materialInst_renderDebug <- getMaterialInstanceData _resources "render_debug"
                beginRenderPassPipeline rendererData commandBuffer swapChainIndex materialInst_renderDebug
                let descriptorOffset = 0
                    writeDescriptorSetPtr = ptrAtIndex ((_writeDescriptorSetPtrs materialInst_renderDebug) !! swapChainIndex) descriptorOffset
                    descriptorWriteCount = 1
                imageInfo <- getRenderTarget rendererData debugRenderTarget
                withPtr (Texture._descriptorImageInfo imageInfo) $ \imageInfoPtr -> do
                    writeField @"pImageInfo" writeDescriptorSetPtr imageInfoPtr
                vkUpdateDescriptorSets _device descriptorWriteCount writeDescriptorSetPtr 0 VK_NULL
                drawElements rendererData commandBuffer quadGeometryData

            -- End command buffer
            vkEndCommandBuffer commandBuffer >>= flip validationVK "vkEndCommandBuffer failed!"

            -- End Render
            presentResult <- presentSwapChain rendererData commandBufferPtr frameFencePtr imageAvailableSemaphore renderFinishedSemaphore
            return presentResult
        otherwise -> return acquireNextImageResult

    let needRecreateSwapChain = (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result)
    writeIORef _needRecreateSwapChainRef needRecreateSwapChain
    writeIORef _frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount


renderSolid :: RendererData
            -> VkCommandBuffer
            -> Int
            -> SceneManager.SceneManagerData
            -> IO ()
renderSolid rendererData commandBuffer swapChainIndex sceneManagerData = do
    staticObjectRenderElements <- SceneManager.getStaticObjectRenderElements sceneManagerData
    forM_ (zip [(0::Int)..] staticObjectRenderElements) $ \(index, renderElement) -> do
        let renderObject = RenderElement._renderObject renderElement
            geometryBufferData = RenderElement._geometryData renderElement
            vertexBufferPtr = _vertexBufferPtr geometryBufferData
            indexBuffer = _indexBuffer geometryBufferData
            indexCount = _vertexIndexCount geometryBufferData
            materialInstanceData = RenderElement._materialInstanceData renderElement
            renderPassData = _renderPassData materialInstanceData

        Just frameBufferData <- getFrameBufferData (_resources rendererData) (RenderPass._renderPassFrameBufferName (renderPassData::RenderPass.RenderPassData))
        let renderPassBeginInfo = atSwapChainIndex swapChainIndex (_renderPassBeginInfos frameBufferData)
            descriptorSetPtr = _descriptorSetsPtr materialInstanceData
            pipelineData = RenderPass._defaultPipelineData renderPassData
            pipelineLayout = RenderPass._pipelineLayout pipelineData
            pipeline = RenderPass._pipeline pipelineData

        when (0 == index) $ do
            -- begin renderpass
            withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
                vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE

            -- set viewport
            withPtr (_frameBufferViewPort . _frameBufferInfo $ frameBufferData) $ \viewPortPtr ->
                vkCmdSetViewport commandBuffer 0 1 viewPortPtr
            withPtr (_frameBufferScissorRect . _frameBufferInfo $ frameBufferData) $ \scissorRectPtr ->
                vkCmdSetScissor commandBuffer 0 1 scissorRectPtr

            -- bind pipeline
            vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline

        -- bind descriptorset
        vkCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 (ptrAtIndex descriptorSetPtr swapChainIndex) 0 VK_NULL

        -- update model view matrix
        modelMatrix <- TransformObject.getMatrix (RenderObject._transformObject renderObject)
        let pushConstantData = PushConstantData { modelMatrix = modelMatrix }

        with pushConstantData $ \pushConstantDataPtr ->
            vkCmdPushConstants commandBuffer pipelineLayout VK_SHADER_STAGE_ALL 0 (bSizeOf pushConstantData) (castPtr pushConstantDataPtr)

        -- drawing commands
        vkCmdBindVertexBuffers commandBuffer 0 1 vertexBufferPtr (_vertexOffsetPtr rendererData)

        vkCmdBindIndexBuffer commandBuffer indexBuffer 0 VK_INDEX_TYPE_UINT32
        vkCmdDrawIndexed commandBuffer indexCount 1 0 0 0
    vkCmdEndRenderPass commandBuffer


renderShadow :: RendererData
             -> VkCommandBuffer
             -> Int
             -> SceneManager.SceneManagerData
             -> IO ()
renderShadow rendererData commandBuffer swapChainIndex sceneManagerData = do
    materialInst_renderShadow <- getMaterialInstanceData (_resources rendererData) "render_shadow"
    -- beginRenderPassPipeline rendererData commandBuffer swapChainIndex materialInst_renderShadow

    let renderPassData = _renderPassData materialInst_renderShadow
    Just frameBufferData <- getFrameBufferData (_resources rendererData) (RenderPass._renderPassFrameBufferName (renderPassData::RenderPass.RenderPassData))
    let renderPassBeginInfo = atSwapChainIndex swapChainIndex (_renderPassBeginInfos frameBufferData)
        descriptorSetPtr = _descriptorSetsPtr materialInst_renderShadow
        pipelineData = RenderPass._defaultPipelineData renderPassData
        pipelineLayout = RenderPass._pipelineLayout pipelineData
        pipeline = RenderPass._pipeline pipelineData

    -- begin renderpass
    withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
        vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE
    withPtr (_frameBufferViewPort . _frameBufferInfo $ frameBufferData) $ \viewPortPtr ->
        vkCmdSetViewport commandBuffer 0 1 viewPortPtr
    withPtr (_frameBufferScissorRect . _frameBufferInfo $ frameBufferData) $ \scissorRectPtr ->
        vkCmdSetScissor commandBuffer 0 1 scissorRectPtr
    vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
    vkCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 (ptrAtIndex descriptorSetPtr swapChainIndex) 0 VK_NULL

    staticObjectRenderElements <- SceneManager.getStaticObjectRenderElements sceneManagerData
    forM_ (zip [(0::Int)..] staticObjectRenderElements) $ \(index, renderElement) -> do
        let renderObject = RenderElement._renderObject renderElement
            geometryBufferData = RenderElement._geometryData renderElement
            vertexBufferPtr = _vertexBufferPtr geometryBufferData
            indexBuffer = _indexBuffer geometryBufferData
            indexCount = _vertexIndexCount geometryBufferData

        -- update model view matrix
        modelMatrix <- TransformObject.getMatrix (RenderObject._transformObject renderObject)
        let pushConstantData = PushConstantData { modelMatrix = modelMatrix }

        with pushConstantData $ \pushConstantDataPtr ->
            vkCmdPushConstants commandBuffer pipelineLayout VK_SHADER_STAGE_ALL 0 (bSizeOf pushConstantData) (castPtr pushConstantDataPtr)

        -- drawing commands
        vkCmdBindVertexBuffers commandBuffer 0 1 vertexBufferPtr (_vertexOffsetPtr rendererData)

        vkCmdBindIndexBuffer commandBuffer indexBuffer 0 VK_INDEX_TYPE_UINT32
        vkCmdDrawIndexed commandBuffer indexCount 1 0 0 0
    vkCmdEndRenderPass commandBuffer


renderPostProcess :: RendererData
                  -> VkCommandBuffer
                  -> Int
                  -> GeometryData
                  -> IO ()
renderPostProcess rendererData@RendererData {..} commandBuffer swapChainIndex quadGeometryData = do
    materialInst_renderSSAO <- getMaterialInstanceData _resources "render_ssao"
    materialInst_compositeGBuffer <- getMaterialInstanceData _resources "composite_gbuffer"

    -- SSAO
    beginRenderPassPipeline rendererData commandBuffer swapChainIndex materialInst_renderSSAO
    drawElements rendererData commandBuffer quadGeometryData

    -- Composite GBuffer
    beginRenderPassPipeline rendererData commandBuffer swapChainIndex materialInst_compositeGBuffer
    drawElements rendererData commandBuffer quadGeometryData
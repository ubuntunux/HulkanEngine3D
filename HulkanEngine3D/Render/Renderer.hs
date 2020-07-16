{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
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
    , _imageIndexPtr :: Ptr Word32
    , _needRecreateSwapChainRef :: IORef Bool
    , _imageAvailableSemaphores :: [VkSemaphore]
    , _renderFinishedSemaphores :: [VkSemaphore]
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
    , _renderTargetDataMap :: RenderTargetDataMap
    , _uniformBufferDataMap :: UniformBufferDataMap
    , _resources :: Resources
    } deriving (Show)


class RendererInterface a where
    getPhysicalDevice :: a -> VkPhysicalDevice
    getDevice :: a -> VkDevice
    getSwapChainData :: a -> IO SwapChainData
    getSwapChainImageViews :: a -> IO [VkImageView]
    getSwapChainSupportDetails :: a -> IO SwapChainSupportDetails
    getCommandPool :: a -> VkCommandPool
    getCommandBuffers :: a -> IO [VkCommandBuffer]
    getCommandBuffer :: a -> Int -> IO VkCommandBuffer
    getGraphicsQueue :: a -> VkQueue
    getPresentQueue :: a -> VkQueue
    getUniformBufferData :: a -> UniformBufferType -> IO UniformBufferData
    createRenderTarget :: a -> Text.Text -> Texture.TextureCreateInfo -> IO Texture.TextureData
    createTexture :: a -> Text.Text -> Texture.TextureCreateInfo -> IO Texture.TextureData
    destroyTexture :: a -> Texture.TextureData -> IO ()
    getRenderTarget :: a -> RenderTargetType -> IO Texture.TextureData
    createGeometryBuffer :: a -> Text.Text -> GeometryCreateInfo -> IO GeometryData
    destroyGeometryBuffer :: a -> GeometryData -> IO ()
    deviceWaitIdle :: a -> IO ()

instance RendererInterface RendererData where
    getPhysicalDevice rendererData = (_physicalDevice rendererData)
    getDevice rendererData = (_device rendererData)
    getSwapChainData rendererData = readIORef $ _swapChainDataRef rendererData
    getSwapChainImageViews rendererData = _swapChainImageViews <$> getSwapChainData rendererData
    getSwapChainSupportDetails rendererData = readIORef $ _swapChainSupportDetailsRef rendererData
    getCommandPool rendererData = (_commandPool rendererData)
    getCommandBuffers rendererData = peekArray (_commandBufferCount rendererData) (_commandBuffersPtr rendererData)
    getCommandBuffer rendererData index = do
        commandBuffers <- getCommandBuffers rendererData
        return $ commandBuffers !! index
    getGraphicsQueue rendererData = (_graphicsQueue (_queueFamilyDatas rendererData))
    getPresentQueue rendererData = (_presentQueue (_queueFamilyDatas rendererData))

    getUniformBufferData :: RendererData -> UniformBufferType -> IO UniformBufferData
    getUniformBufferData rendererData uniformBufferType =
        Maybe.fromJust <$> HashTable.lookup (_uniformBufferDataMap rendererData) uniformBufferType

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
            , _swapChainImages = []
            , _swapChainImageFormat = VK_FORMAT_UNDEFINED
            , _swapChainImageViews = []
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

    imageIndexPtr <- new (0 :: Word32)
    frameIndexRef <- newIORef (0::Int)
    needRecreateSwapChainRef <- newIORef False
    imageSamplers <- newIORef defaultImageSamplers
    renderPassDataListRef <- newIORef (DList.fromList [])
    swapChainDataRef <- newIORef defaultSwapChainData
    swapChainSupportDetailsRef <- newIORef defaultSwapChainSupportDetails
    renderTargetDataMap <- HashTable.new
    uniformBufferDataMap <- HashTable.new

    return RendererData
        { _frameIndexRef = frameIndexRef
        , _imageIndexPtr = imageIndexPtr
        , _needRecreateSwapChainRef = needRecreateSwapChainRef
        , _imageAvailableSemaphores = []
        , _renderFinishedSemaphores = []
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
        , _renderTargetDataMap = renderTargetDataMap
        , _uniformBufferDataMap = uniformBufferDataMap
        , _resources = resources
        }

initializeRenderer :: RendererData -> IO RendererData
initializeRenderer rendererData@RendererData {..} = do
    poke _imageIndexPtr 0
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
    free _imageIndexPtr


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



renderScene :: RendererData -> SceneManager.SceneManagerData -> Double -> Float -> IO ()
renderScene rendererData@RendererData{..} sceneManagerData elapsedTime deltaTime = do
    -- frame index
    frameIndex <- readIORef _frameIndexRef
    let frameFencePtr = ptrAtIndex _frameFencesPtr frameIndex
        imageAvailableSemaphore = _imageAvailableSemaphores !! frameIndex
        renderFinishedSemaphore = _renderFinishedSemaphores !! frameIndex
    swapChainData@SwapChainData {..} <- getSwapChainData rendererData

    -- Begin Render
    acquireNextImageResult <- vkAcquireNextImageKHR _device _swapChain maxBound imageAvailableSemaphore VK_NULL_HANDLE _imageIndexPtr
    imageIndexValue <- peek _imageIndexPtr
    commandBuffers <- getCommandBuffers rendererData
    let imageIndex = fromIntegral imageIndexValue
        commandBufferPtr = ptrAtIndex _commandBuffersPtr imageIndex
        commandBuffer = commandBuffers !! imageIndex

    result <- case acquireNextImageResult of
        VK_SUCCESS -> do
            mainCamera <- SceneManager.getMainCamera sceneManagerData
            mainLight <- SceneManager.getMainLight sceneManagerData
            viewMatrix <- Camera.getViewMatrix mainCamera
            projectionMatrix <- Camera.getProjectionMatrix mainCamera
            viewProjectionMatrix <- Camera.getViewProjectionMatrix mainCamera
            invViewProjectionMatrix <- Camera.getInvViewProjectionMatrix mainCamera
            let screenWidth = fromIntegral $ getField @"width" _swapChainExtent :: Float
                screenHeight = fromIntegral $ getField @"height" _swapChainExtent :: Float

            shadowViewProjectionMatrix <- Light.getShadowViewProjectionMatrix mainLight
            lightPosition <- Light.getLightPosition mainLight
            lightDirection <- Light.getLightDirection mainLight
            lightColor <- Light.getLightColor mainLight
            shadowExp <- Light.getLightShadowExp mainLight
            shadowBias <- Light.getLightShadowBias mainLight
            shadowSamples <- Light.getLightShadowSamples mainLight

            rotation <- TransformObject.getRotation $ Light._directionalLightTransformObject mainLight

            -- Upload Uniform Buffers
            sceneConstantsBufferData <- getUniformBufferData rendererData UniformBuffer_SceneConstants
            viewProjectionConstantsBufferData <- getUniformBufferData rendererData UniformBuffer_ViewProjectionConstants
            lightConstantsBufferData <- getUniformBufferData rendererData UniformBuffer_LightConstants
            let sceneConstantsBufferMemory = (_uniformBufferMemories sceneConstantsBufferData) !! imageIndex
                viewProjectionConstantsBufferMemory = (_uniformBufferMemories viewProjectionConstantsBufferData) !! imageIndex
                lightConstantsBufferMemory = (_uniformBufferMemories lightConstantsBufferData) !! imageIndex
                sceneConstants = SceneConstants
                    { _SCREEN_SIZE = vec2 screenWidth screenHeight
                    , _BACKBUFFER_SIZE = vec2 screenWidth screenHeight
                    , _TIME = realToFrac elapsedTime
                    , _DELTA_TIME = scalar deltaTime
                    , _JITTER_FRAME = float_zero
                    , _SceneConstantsDummy0 = 0
                    }
                viewProjectionConstants = ViewProjectionConstants
                    { _VIEW = viewMatrix
                    , _PROJECTION = projectionMatrix
                    , _VIEW_PROJECTION = viewProjectionMatrix
                    , _INV_VIEW_PROJECTION = invViewProjectionMatrix
                    }
                lightConstants = LightConstants
                    { _SHADOW_VIEW_PROJECTION = shadowViewProjectionMatrix
                    , _LIGHT_POSITION = lightPosition
                    , _SHADOW_EXP = scalar shadowExp
                    , _LIGHT_DIRECTION = lightDirection
                    , _SHADOW_BIAS = scalar shadowBias
                    , _LIGHT_COLOR = lightColor
                    , _SHADOW_SAMPLES = scalar shadowSamples
                    }
            updateBufferData _device sceneConstantsBufferMemory sceneConstants
            updateBufferData _device viewProjectionConstantsBufferMemory viewProjectionConstants
            updateBufferData _device lightConstantsBufferMemory lightConstants

            -- Begin command buffer
            let commandBufferBeginInfo = createVk @VkCommandBufferBeginInfo
                    $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                    &* set @"pNext" VK_NULL
                    &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
            withPtr commandBufferBeginInfo $ \commandBufferBeginInfoPtr -> do
                result <- vkBeginCommandBuffer commandBuffer commandBufferBeginInfoPtr
                validationVK result "vkBeginCommandBuffer failed!"

            -- Render
            renderSolid rendererData commandBuffer imageIndex sceneManagerData

            quadMeshData <- getMeshData _resources "quad"
            quadGeometryBufferData <- Mesh.getGeometryData quadMeshData 0
            materialInst_renderSSAO <- getMaterialInstanceData _resources "render_ssao"
            materialInst_compositeGBuffer <- getMaterialInstanceData _resources "composite_gbuffer"
            materialInst_renderDebug <- getMaterialInstanceData _resources "render_debug"

            renderPostProcess rendererData commandBuffer imageIndex quadGeometryBufferData materialInst_renderSSAO
            renderPostProcess rendererData commandBuffer imageIndex quadGeometryBufferData materialInst_compositeGBuffer
            renderPostProcess rendererData commandBuffer imageIndex quadGeometryBufferData materialInst_renderDebug

            -- End command buffer
            vkEndCommandBuffer commandBuffer >>= flip validationVK "vkEndCommandBuffer failed!"

            -- End Render
            presentResult <- presentSwapChain rendererData commandBufferPtr frameFencePtr imageAvailableSemaphore renderFinishedSemaphore
            return presentResult
        otherwise -> return acquireNextImageResult

    let needRecreateSwapChain = (VK_ERROR_OUT_OF_DATE_KHR == result || VK_SUBOPTIMAL_KHR == result)
    writeIORef _needRecreateSwapChainRef needRecreateSwapChain
    writeIORef _frameIndexRef $ mod (frameIndex + 1) Constants.maxFrameCount


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


renderSolid :: RendererData
            -> VkCommandBuffer
            -> Int
            -> SceneManager.SceneManagerData
            -> IO ()
renderSolid rendererData commandBuffer imageIndex sceneManagerData = do
    staticObjectRenderElements <- SceneManager.getStaticObjectRenderElements sceneManagerData
    forM_ (zip [(0::Int)..] staticObjectRenderElements) $ \(index, renderElement) -> do
        let renderObject = RenderElement._renderObject renderElement
            geometryBufferData = RenderElement._geometryData renderElement
            vertexBuffer = _vertexBuffer geometryBufferData
            indexBuffer = _indexBuffer geometryBufferData
            indexCount = _vertexIndexCount geometryBufferData
            materialInstanceData = RenderElement._materialInstanceData renderElement
            renderPassData = _renderPassData materialInstanceData

        Just frameBufferData <- getFrameBufferData (_resources rendererData) (RenderPass._renderPassFrameBufferName (renderPassData::RenderPass.RenderPassData))
        let renderPassBeginInfo = (_renderPassBeginInfos frameBufferData) !! imageIndex
            descriptorSet = (_descriptorSets materialInstanceData) !! imageIndex
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
        with descriptorSet $ \descriptorSetPtr ->
            vkCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 descriptorSetPtr 0 VK_NULL

        -- update model view matrix
        modelMatrix <- TransformObject.getMatrix (RenderObject._transformObject renderObject)
        let pushConstantData = PushConstantData { modelMatrix = modelMatrix }

        with modelMatrix $ \modelMatrixPtr ->
            vkCmdPushConstants commandBuffer pipelineLayout VK_SHADER_STAGE_ALL 0 (bSizeOf pushConstantData) (castPtr modelMatrixPtr)

        -- drawing commands
        with vertexBuffer $ \vertexBufferPtr ->
            with 0 $ \vertexOffsetPtr ->
                vkCmdBindVertexBuffers commandBuffer 0 1 vertexBufferPtr vertexOffsetPtr

        vkCmdBindIndexBuffer commandBuffer indexBuffer 0 VK_INDEX_TYPE_UINT32
        vkCmdDrawIndexed commandBuffer indexCount 1 0 0 0
    vkCmdEndRenderPass commandBuffer


renderPostProcess :: RendererData
                  -> VkCommandBuffer
                  -> Int
                  -> GeometryData
                  -> MaterialInstanceData
                  -> IO ()
renderPostProcess rendererData commandBuffer imageIndex geometryBufferData materialInstanceData = do
    let vertexBuffer = _vertexBuffer geometryBufferData
        indexBuffer = _indexBuffer geometryBufferData
        indexCount = _vertexIndexCount geometryBufferData
        renderPassData = _renderPassData materialInstanceData

    Just frameBufferData <- getFrameBufferData (_resources rendererData) (RenderPass.getRenderPassFrameBufferName renderPassData)
    let renderPassBeginInfo = (_renderPassBeginInfos frameBufferData) !! imageIndex
        descriptorSet = (_descriptorSets materialInstanceData) !! imageIndex
        pipelineData = RenderPass.getDefaultPipelineData renderPassData
        pipelineLayout = RenderPass._pipelineLayout pipelineData
        pipeline = RenderPass._pipeline pipelineData

    withPtr renderPassBeginInfo $ \renderPassBeginInfoPtr ->
        vkCmdBeginRenderPass commandBuffer renderPassBeginInfoPtr VK_SUBPASS_CONTENTS_INLINE

    withPtr (_frameBufferViewPort . _frameBufferInfo $ frameBufferData) $ \viewPortPtr ->
        vkCmdSetViewport commandBuffer 0 1 viewPortPtr
    withPtr (_frameBufferScissorRect . _frameBufferInfo $ frameBufferData) $ \scissorRectPtr ->
        vkCmdSetScissor commandBuffer 0 1 scissorRectPtr

    vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline

    with descriptorSet $ \descriptorSetPtr ->
        vkCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 descriptorSetPtr 0 VK_NULL

    with vertexBuffer $ \vertexBufferPtr ->
        with 0 $ \vertexOffsetPtr ->
            vkCmdBindVertexBuffers commandBuffer 0 1 vertexBufferPtr vertexOffsetPtr

    vkCmdBindIndexBuffer commandBuffer indexBuffer 0 VK_INDEX_TYPE_UINT32
    vkCmdDrawIndexed commandBuffer indexCount 1 0 0 0
    vkCmdEndRenderPass commandBuffer


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
          &* set @"pImageIndices" _imageIndexPtr
          &* set @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [renderFinishedSemaphore]
          &* set @"swapchainCount" 1
          &* setListRef @"pSwapchains" [_swapChain]

    result <- withPtr presentInfo $ \presentInfoPtr -> do
        vkQueuePresentKHR _presentQueue presentInfoPtr

    -- waiting
    deviceWaitIdle rendererData

    return result
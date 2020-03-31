{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.Resource
    ( ResourceData (..)
    , ResourceInterface (..)
    ) where

import Data.IORef
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.Vector.Mutable as MVector

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Mesh
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Resource.ObjLoader
import HulkanEngine3D.Vulkan
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.SwapChain

type MeshDataMap = HashTable.BasicHashTable Text.Text MeshData
type TextureDataMap = HashTable.BasicHashTable Text.Text TextureData
type RenderPassDataMap = HashTable.BasicHashTable Text.Text RenderPassData

data ResourceData = ResourceData
    { _meshDataMap :: MeshDataMap
    , _textureDataMap :: TextureDataMap
    , _renderPassDataMap :: RenderPassDataMap
    } deriving (Show)


class ResourceInterface a where
    createNewResourceData :: IO a
    initializeResourceData :: a -> RendererData -> IO ()
    destroyResourceData :: a -> RendererData -> IO ()

    loadMeshDatas :: a -> RendererData -> IO ()
    unloadMeshDatas :: a -> RendererData -> IO ()
    getMeshData :: a -> Text.Text -> IO (Maybe MeshData)

    loadTextureDatas :: a -> RendererData -> IO ()
    unloadTextureDatas :: a -> RendererData -> IO ()
    getTextureData :: a -> Text.Text -> IO (Maybe TextureData)

    loadRenderPassDatas :: a -> RendererData -> IO ()
    unloadRenderPassDatas :: a -> RendererData -> IO ()
    getRenderPassData :: a -> Text.Text -> IO (Maybe RenderPassData)
    getDefaultRenderPassData :: a -> IO RenderPassData

instance ResourceInterface ResourceData where
    createNewResourceData :: IO ResourceData
    createNewResourceData = do
        meshDataMap <- HashTable.new
        textureDataMap <- HashTable.new
        renderPassDataMap <- HashTable.new
        return ResourceData
            { _meshDataMap = meshDataMap
            , _textureDataMap = textureDataMap
            , _renderPassDataMap = renderPassDataMap
            }

    initializeResourceData :: ResourceData -> RendererData -> IO ()
    initializeResourceData resourceData rendererData = do
        loadMeshDatas resourceData rendererData
        loadTextureDatas resourceData rendererData
        loadRenderPassDatas resourceData rendererData

    destroyResourceData :: ResourceData -> RendererData -> IO ()
    destroyResourceData resourceData rendererData = do
        unloadMeshDatas resourceData rendererData
        unloadTextureDatas resourceData rendererData
        unloadRenderPassDatas resourceData rendererData

    -- Mesh Loader
    loadMeshDatas :: ResourceData -> RendererData -> IO ()
    loadMeshDatas resourceData rendererData = do
        let name = "suzan"::Text.Text
        (vertices, indices) <- loadModel "Resource/Externals/Meshes/suzan.obj"
        geometryBufferData <- createGeometryBuffer rendererData name vertices indices
        meshData <- newMeshData name [geometryBufferData]
        HashTable.insert (_meshDataMap resourceData) name meshData

    unloadMeshDatas :: ResourceData -> RendererData -> IO ()
    unloadMeshDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> (action rendererData k v)) (_meshDataMap resourceData)
        where
            action rendererData name meshData = do
                let count = MVector.length (_geometryBufferDatas meshData)
                    loop x
                        | x < count = do
                            geometryBufferData <- MVector.unsafeRead (_geometryBufferDatas meshData) x
                            destroyGeometryBuffer rendererData geometryBufferData
                            loop (x+1)
                        | otherwise = return ()
                loop 0

    getMeshData :: ResourceData -> Text.Text -> IO (Maybe MeshData)
    getMeshData resourceData resourceName = do
        HashTable.lookup (_meshDataMap resourceData) resourceName

    -- TextureLoader
    loadTextureDatas :: ResourceData -> RendererData -> IO ()
    loadTextureDatas resourceData rendererData = do
        textureData <- createTexture rendererData "Resource/Externals/Textures/texture.jpg"
        HashTable.insert (_textureDataMap resourceData) "texture" textureData

    unloadTextureDatas :: ResourceData -> RendererData -> IO ()
    unloadTextureDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> destroyTexture rendererData v) (_textureDataMap resourceData)

    getTextureData :: ResourceData -> Text.Text -> IO (Maybe TextureData)
    getTextureData resourceData resourceName = do
        HashTable.lookup (_textureDataMap resourceData) resourceName

    -- RenderPassLoader
    loadRenderPassDatas :: ResourceData -> RendererData -> IO ()
    loadRenderPassDatas resourceData rendererData = do
        renderTargets <- readIORef (_renderTargets rendererData)
        swapChainData <- readIORef (_swapChainDataRef rendererData)

        let msaaSampleCount = (_msaaSamples . _renderFeatures $ rendererData)
            renderPassImageAttachmentDescriptions =
                [ defaultAttachmentDescription
                    { _attachmentImageFormat = (_imageFormat . _sceneColorTexture $ renderTargets)
                    , _attachmentImageSamples = msaaSampleCount
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                    }
                , defaultAttachmentDescription
                    { _attachmentImageFormat = (_imageFormat . _sceneDepthTexture $ renderTargets)
                    , _attachmentImageSamples = msaaSampleCount
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_DONT_CARE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                    }
                , defaultAttachmentDescription
                    { _attachmentImageFormat = (_imageFormat . _sceneColorTexture $ renderTargets)
                    , _attachmentImageSamples = VK_SAMPLE_COUNT_1_BIT
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_DONT_CARE
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                    }
                ]

        let renderPassDataCreateInfo = RenderPassDataCreateInfo
                { _vertexShaderFile = "Resource/Shaders/triangle.vert"
                , _fragmentShaderFile = "Resource/Shaders/triangle.frag"
                , _renderPassSwapChainImageCount = fromIntegral Constants.swapChainImageCount
                , _renderPassImageAttachmentDescriptions = renderPassImageAttachmentDescriptions
                , _renderPassImageWidth = _imageWidth._sceneColorTexture $ renderTargets
                , _renderPassImageHeight = _imageHeight._sceneColorTexture $ renderTargets
                , _renderPassImageDepth = _imageDepth._sceneColorTexture $ renderTargets
                , _renderPassImageViewsList = [
                    [ _imageView._sceneColorTexture $ renderTargets
                    , _imageView._sceneDepthTexture $ renderTargets
                    , (_swapChainImageViews swapChainData) !! index
                    ] | index <- Constants.swapChainImageIndices]
                , _renderPassSampleCount = msaaSampleCount
                , _renderPassClearValues = [getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0]
                }
        renderPassData <- createRenderPassData (getDevice rendererData) renderPassDataCreateInfo
        HashTable.insert (_renderPassDataMap resourceData) "defaultRenderPass" renderPassData

    unloadRenderPassDatas :: ResourceData -> RendererData -> IO ()
    unloadRenderPassDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> destroyRenderPassData (getDevice rendererData) v) (_renderPassDataMap resourceData)

    getRenderPassData :: ResourceData -> Text.Text -> IO (Maybe RenderPassData)
    getRenderPassData resourceData resourceName = do
        HashTable.lookup (_renderPassDataMap resourceData) resourceName

    getDefaultRenderPassData :: ResourceData -> IO RenderPassData
    getDefaultRenderPassData resourceData = do
        Just renderPassData <- getRenderPassData resourceData "defaultRenderPass"
        return renderPassData
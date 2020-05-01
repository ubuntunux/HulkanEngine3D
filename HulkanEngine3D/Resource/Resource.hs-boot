module HulkanEngine3D.Resource.Resource where

import qualified Data.Text as Text

import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.Mesh
import qualified HulkanEngine3D.Render.Model as Model
import HulkanEngine3D.Vulkan.Descriptor
import {-# SOURCE #-} HulkanEngine3D.Render.Renderer
import {-# SOURCE #-} HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.Texture

data ResourceData

instance Show ResourceData where

class ResourceInterface a where
    createNewResourceData :: IO a
    initializeResourceData :: a -> RendererData -> IO ()
    destroyResourceData :: a -> RendererData -> IO ()

    loadGraphicsDatas :: a -> RendererData -> IO ()
    unloadGraphicsDatas :: a -> RendererData -> IO ()

    loadSceneManagerDatas :: a -> RendererData -> IO ()
    unloadSceneManagerDatas :: a -> RendererData -> IO ()

    loadModelDatas :: a -> RendererData -> IO ()
    unloadModelDatas :: a -> RendererData -> IO ()
    getModelData :: a -> Text.Text -> IO Model.ModelData

    loadMeshDatas :: a -> RendererData -> IO ()
    unloadMeshDatas :: a -> RendererData -> IO ()
    getMeshData :: a -> Text.Text -> IO MeshData

    loadTextureDatas :: a -> RendererData -> IO ()
    unloadTextureDatas :: a -> RendererData -> IO ()
    getTextureData :: a -> Text.Text -> IO TextureData

    loadFrameBufferDatas :: a -> RendererData -> IO ()
    unloadFrameBufferDatas :: a -> RendererData -> IO ()
    getFrameBufferData :: a -> Text.Text -> IO (Maybe FrameBufferData)

    loadRenderPassDatas :: a -> RendererData -> IO ()
    unloadRenderPassDatas :: a -> RendererData -> IO ()
    getRenderPassData :: a -> Text.Text -> IO (Maybe RenderPassData)
    getDefaultRenderPassData :: a -> IO (Maybe RenderPassData)

    loadMaterialInstanceDatas :: a -> RendererData -> IO ()
    unloadMaterialInstanceDatas :: a -> RendererData -> IO ()
    getMaterialInstanceData :: a -> Text.Text -> IO MaterialInstanceData

    getDescriptorData :: a -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO DescriptorData
    unloadDescriptorDatas :: a -> RendererData -> IO ()

instance ResourceInterface ResourceData where
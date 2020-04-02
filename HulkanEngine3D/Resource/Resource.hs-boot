module HulkanEngine3D.Resource.Resource where

import qualified Data.Text as Text

import HulkanEngine3D.Render.Mesh
import {-# SOURCE #-} HulkanEngine3D.Render.Renderer
import {-# SOURCE #-} HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.Texture

data ResourceData

instance Show ResourceData where

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
    getDefaultRenderPassData :: a -> IO (Maybe RenderPassData)

instance ResourceInterface ResourceData where
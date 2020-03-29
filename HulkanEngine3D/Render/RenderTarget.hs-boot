module HulkanEngine3D.Render.RenderTarget where

import {-# SOURCE #-} HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Texture

data RenderTargets = RenderTargets
    { _sceneColorTexture :: TextureData
    , _sceneDepthTexture :: TextureData
    }

createRenderTargets :: RendererData -> IO RenderTargets

destroyRenderTargets :: RendererData -> RenderTargets -> IO ()
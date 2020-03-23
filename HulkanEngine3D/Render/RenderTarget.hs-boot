module HulkanEngine3D.Render.RenderTarget where

import {-# SOURCE #-} HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Texture

data RenderTargetData = RenderTargetData
    { _sceneColorTexture :: TextureData
    , _sceneDepthTexture :: TextureData
    }

createRenderTargets :: RendererData -> IO RenderTargetData

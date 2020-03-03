module HulkanEngine3D.Render.RenderTarget where

import HulkanEngine3D.Vulkan.Texture
import {-# SOURCE #-} HulkanEngine3D.Vulkan

data RenderTargetData = RenderTargetData
    { _sceneColorTexture :: TextureData
    , _sceneDepthTexture :: TextureData
    }

createRenderTargets :: RendererData -> IO RenderTargetData

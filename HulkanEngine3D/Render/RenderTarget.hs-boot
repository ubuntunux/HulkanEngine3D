module HulkanEngine3D.Render.RenderTarget where

import {-# SOURCE #-} HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTargetType

createRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
destroyRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
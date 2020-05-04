module HulkanEngine3D.Render.RenderTarget where

import {-# SOURCE #-} HulkanEngine3D.Render.Renderer

data RenderTargets

createRenderTargets :: RendererData -> IO RenderTargets

destroyRenderTargets :: RendererData -> RenderTargets -> IO ()
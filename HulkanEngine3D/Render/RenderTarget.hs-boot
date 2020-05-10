module HulkanEngine3D.Render.RenderTarget where

import qualified Data.Text as Text
import qualified Data.HashTable.IO as HashTable

import {-# SOURCE #-} HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Texture

type RenderTargetDataMap = HashTable.BasicHashTable Text.Text TextureData

createRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()

destroyRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
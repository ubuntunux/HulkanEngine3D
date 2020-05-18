{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Render.RenderTarget where

import qualified Data.Text as Text
import qualified Data.HashTable.IO as HashTable

import Graphics.Vulkan.Core_1_0

import HulkanEngine3D.Render.Renderer
--import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.SwapChain
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import HulkanEngine3D.Utilities.System


type RenderTargetDataMap = HashTable.BasicHashTable Text.Text Texture.TextureData

createRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
createRenderTargets rendererData renderTargetDataMap = do
    swapChainData <- getSwapChainData rendererData
    let windowWidth = getField @"width" (_swapChainExtent swapChainData)
        windowHeight = getField @"height" (_swapChainExtent swapChainData)
        samples = VK_SAMPLE_COUNT_1_BIT -- min VK_SAMPLE_COUNT_4_BIT (_msaaSamples . _renderFeatures $ rendererData)
    registRenderTarget rendererData renderTargetDataMap "SceneColor" $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R16G16B16A16_SFLOAT
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            False
            False
    registRenderTarget rendererData renderTargetDataMap "SceneDepth" $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_D32_SFLOAT
            samples
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            False
            False
    registRenderTarget rendererData renderTargetDataMap "BackBuffer" $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_B8G8R8A8_UNORM
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            False
            False
    where
        registRenderTarget :: RendererData -> RenderTargetDataMap -> Text.Text -> Texture.RenderTargetCreateInfo -> IO ()
        registRenderTarget rendererData renderTargetDataMap renderTargetName renderTargetCreateInfo = do
            textureData <- createRenderTarget rendererData renderTargetName renderTargetCreateInfo
            HashTable.insert renderTargetDataMap renderTargetName textureData

destroyRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
destroyRenderTargets rendererData renderTargetDataMap =
    clearHashTable renderTargetDataMap (\(k, v) -> destroyTexture rendererData v)
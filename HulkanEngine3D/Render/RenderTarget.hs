{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Render.RenderTarget where

import Control.Monad
import qualified Data.Text as Text
import qualified Data.HashTable.IO as HashTable

import Graphics.Vulkan.Core_1_0

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.SwapChain
import qualified HulkanEngine3D.Vulkan.Texture as Texture

type RenderTargetDataMap = HashTable.BasicHashTable Text.Text Texture.TextureData

createRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
createRenderTargets rendererData renderTargetDataMap = do
    swapChainData <- getSwapChainData rendererData
    let windowSize = _swapChainExtent swapChainData
        samples = min VK_SAMPLE_COUNT_4_BIT (_msaaSamples . _renderFeatures $ rendererData)
    registRenderTarget rendererData renderTargetDataMap "SceneColor" VK_FORMAT_B8G8R8A8_UNORM windowSize samples VK_FILTER_LINEAR VK_FILTER_LINEAR VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    registRenderTarget rendererData renderTargetDataMap "SceneDepth" VK_FORMAT_D32_SFLOAT windowSize samples VK_FILTER_NEAREST VK_FILTER_NEAREST VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    registRenderTarget rendererData renderTargetDataMap "BackBuffer" VK_FORMAT_B8G8R8A8_UNORM windowSize samples VK_FILTER_LINEAR VK_FILTER_LINEAR VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    where
        registRenderTarget :: RendererData
                           -> RenderTargetDataMap
                           -> Text.Text
                           -> VkFormat
                           -> VkExtent2D
                           -> VkSampleCountFlagBits
                           -> VkFilter
                           -> VkFilter
                           -> VkSamplerAddressMode
                           -> IO ()
        registRenderTarget rendererData renderTargetDataMap renderTargetName format size sampleCount minFilter magFilter wrapMode = do
            textureData <- createRenderTarget rendererData renderTargetName format size sampleCount minFilter magFilter wrapMode
            HashTable.insert renderTargetDataMap renderTargetName textureData

destroyRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
destroyRenderTargets rendererData renderTargetDataMap = do
    HashTable.mapM_ (\(k, v) -> destroyTexture rendererData v) renderTargetDataMap
    elementList <- HashTable.toList renderTargetDataMap
    forM_ elementList (\(k, v) -> HashTable.delete renderTargetDataMap k)
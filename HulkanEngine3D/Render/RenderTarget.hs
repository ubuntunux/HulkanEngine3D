{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module HulkanEngine3D.Render.RenderTarget where

import qualified Data.HashTable.IO as HashTable

import Graphics.Vulkan.Core_1_0

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTargetType
import HulkanEngine3D.Vulkan.SwapChain
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import HulkanEngine3D.Utilities.System

createRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
createRenderTargets rendererData renderTargetDataMap = do
    swapChainData <- getSwapChainData rendererData
    let windowWidth = getField @"width" (_swapChainExtent swapChainData)
        windowHeight = getField @"height" (_swapChainExtent swapChainData)
        samples = VK_SAMPLE_COUNT_1_BIT -- min VK_SAMPLE_COUNT_4_BIT (_msaaSamples . _renderFeatures $ rendererData)
        enableMipmap = True
        disableMipmap = False
        enableAnisotropy = True
        disableAnisotropy = False
        immutable = True
        mutable = False
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneColor $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R16G16B16A16_SFLOAT
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneDepth $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_D32_SFLOAT
            samples
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    registRenderTarget rendererData renderTargetDataMap RenderTarget_BackBuffer $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_B8G8R8A8_UNORM
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneAlbedo $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R8G8B8A8_UNORM
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneMaterial $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R8G8B8A8_UNORM
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneNormal $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R16G16B16A16_SNORM
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneVelocity $
        Texture.RenderTargetCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R16G16_SFLOAT
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SSAO $
        Texture.RenderTargetCreateInfo
            (div windowWidth 2)
            (div windowHeight 2)
            1
            VK_FORMAT_R16_SFLOAT
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
    where
        registRenderTarget :: RendererData -> RenderTargetDataMap -> RenderTargetType -> Texture.RenderTargetCreateInfo -> IO ()
        registRenderTarget rendererData renderTargetDataMap renderTargetType renderTargetCreateInfo = do
            textureData <- createRenderTarget rendererData (toText renderTargetType) renderTargetCreateInfo
            HashTable.insert renderTargetDataMap renderTargetType textureData

destroyRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
destroyRenderTargets rendererData renderTargetDataMap =
    clearHashTable renderTargetDataMap (\(k, v) -> destroyTexture rendererData v)
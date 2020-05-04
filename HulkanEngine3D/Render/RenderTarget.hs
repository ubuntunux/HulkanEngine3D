{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Render.RenderTarget
    ( RenderTargets (..)
    , createRenderTargets
    , destroyRenderTargets
    ) where

import Graphics.Vulkan.Core_1_0

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.SwapChain
import HulkanEngine3D.Vulkan.Texture


data RenderTargets = RenderTargets
    { _sceneColorTexture :: TextureData
    , _sceneDepthTexture :: TextureData
    , _backBufferTexture :: TextureData
    } deriving (Show)


createRenderTargets :: RendererData -> IO RenderTargets
createRenderTargets rendererData = do
    swapChainData <- getSwapChainData rendererData
    let windowSize = _swapChainExtent swapChainData
        samples = min VK_SAMPLE_COUNT_4_BIT (_msaaSamples . _renderFeatures $ rendererData)
    sceneColor <- createRenderTarget rendererData "sceneColor" VK_FORMAT_B8G8R8A8_UNORM windowSize samples
    sceneDepth <- createDepthTarget rendererData "sceneDepth" VK_FORMAT_D32_SFLOAT windowSize samples
    backBuffer <- createRenderTarget rendererData "backBuffer" VK_FORMAT_B8G8R8A8_UNORM windowSize samples
    return RenderTargets
        { _sceneColorTexture = sceneColor
        , _sceneDepthTexture = sceneDepth
        , _backBufferTexture = backBuffer
        }

destroyRenderTargets :: RendererData -> RenderTargets -> IO ()
destroyRenderTargets rendererData renderTargets = do
    destroyTexture rendererData (_sceneColorTexture renderTargets)
    destroyTexture rendererData (_sceneDepthTexture renderTargets)
    destroyTexture rendererData (_backBufferTexture renderTargets)
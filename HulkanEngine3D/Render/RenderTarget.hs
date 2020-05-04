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
    } deriving (Show)


createRenderTargets :: RendererData -> IO RenderTargets
createRenderTargets rendererData = do
    swapChainData <- getSwapChainData rendererData
    let format = _swapChainImageFormat swapChainData
        extent = _swapChainExtent swapChainData
        samples = min VK_SAMPLE_COUNT_4_BIT (_msaaSamples . _renderFeatures $ rendererData)
    sceneColor <- createRenderTarget rendererData "sceneColor" format extent samples
    sceneDepth <- createDepthTarget rendererData "sceneDepth" extent samples
    return RenderTargets
        { _sceneColorTexture = sceneColor
        , _sceneDepthTexture = sceneDepth }

destroyRenderTargets :: RendererData -> RenderTargets -> IO ()
destroyRenderTargets rendererData renderTargets = do
    destroyTexture rendererData (_sceneColorTexture renderTargets)
    destroyTexture rendererData (_sceneDepthTexture renderTargets)
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Render.RenderTarget
    ( RenderTargetData (..)
    , createRenderTargets
    ) where

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan
import HulkanEngine3D.Vulkan.SwapChain
import HulkanEngine3D.Vulkan.Texture


data RenderTargetData = RenderTargetData
    { _sceneColorTexture :: TextureData
    , _sceneDepthTexture :: TextureData
    } deriving (Show)


createRenderTargets :: RendererData -> IO RenderTargetData
createRenderTargets rendererData = do
        swapChainData <- getSwapChainData rendererData
        let format = _swapChainImageFormat swapChainData
            extent = _swapChainExtent swapChainData
            samples = _msaaSamples (_renderFeatures rendererData)
        sceneColor <- createRenderTarget rendererData format extent samples
        sceneDepth <- createDepthTarget rendererData extent samples
        return RenderTargetData
            { _sceneColorTexture = sceneColor
            , _sceneDepthTexture = sceneDepth }
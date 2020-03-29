{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Render.RenderTarget
    ( RenderTargets (..)
    , createRenderTargets
    , destroyRenderTargets
    ) where

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan
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
            samples = _msaaSamples (_renderFeatures rendererData)
        sceneColor <- createRenderTarget rendererData format extent samples
        sceneDepth <- createDepthTarget rendererData extent samples
        return RenderTargets
            { _sceneColorTexture = sceneColor
            , _sceneDepthTexture = sceneDepth }

destroyRenderTargets :: RendererData -> RenderTargets -> IO ()
destroyRenderTargets rendererData renderTargets = do
    destroyTexture rendererData (_sceneColorTexture renderTargets)
    destroyTexture rendererData (_sceneDepthTexture renderTargets)
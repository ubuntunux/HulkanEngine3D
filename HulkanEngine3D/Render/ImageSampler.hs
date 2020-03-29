{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Render.ImageSampler
    ( ImageSamplerData (..)
    , createImageSamplers
    ) where

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan
import HulkanEngine3D.Vulkan.SwapChain
import HulkanEngine3D.Vulkan.Texture


data ImageSamplerData = ImageSamplerData
    { _sceneColorTexture :: TextureData
    , _sceneDepthTexture :: TextureData
    } deriving (Show)


createImageSamplers :: RendererData -> IO ImageSamplerData
createImageSamplers rendererData = do
        swapChainData <- getSwapChainData rendererData
        let format = _swapChainImageFormat swapChainData
            extent = _swapChainExtent swapChainData
            samples = _msaaSamples (_renderFeatures rendererData)
        sceneColor <- createImageSampler rendererData format extent samples
        sceneDepth <- createDepthTarget rendererData extent samples
        return ImageSamplerData
            { _sceneColorTexture = sceneColor
            , _sceneDepthTexture = sceneDepth }
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.FrameBufferCreateInfo where

import Data.IORef
import qualified Data.Text as Text
import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.SwapChain


getFrameBufferDataCreateInfo :: RendererData -> Text.Text -> IO FrameBufferDataCreateInfo
getFrameBufferDataCreateInfo rendererData frameBufferName
    | "default" == frameBufferName = do
        renderTargets@RenderTargets {..} <- readIORef (_renderTargets rendererData)
        swapChainData <- readIORef (_swapChainDataRef rendererData)
        return defaultFrameBufferDataCreateInfo
            { _frameBufferName = frameBufferName
            , _frameBufferWidth = _imageWidth _sceneColorTexture
            , _frameBufferHeight = _imageHeight _sceneColorTexture
            , _frameBufferDepth = _imageDepth _sceneColorTexture
            , _frameBufferSampleCount = _imageSampleCount _sceneColorTexture
            , _frameBufferViewPort = createViewport 0 0 (fromIntegral $ _imageWidth _sceneColorTexture) (fromIntegral $ _imageHeight _sceneColorTexture) 0 1
            , _frameBufferScissorRect = createScissorRect 0 0 (fromIntegral $ _imageWidth _sceneColorTexture) (fromIntegral $ _imageHeight _sceneColorTexture)
            , _frameBufferColorAttachmentFormats = [_imageFormat _sceneColorTexture]
            , _frameBufferDepthAttachmentFormats = [_imageFormat _sceneDepthTexture]
            , _frameBufferResolveAttachmentFormats = [_swapChainImageFormat swapChainData]
            , _frameBufferImageViewsList =
                [[ _imageView _sceneColorTexture
                 , _imageView _sceneDepthTexture
                 , (_swapChainImageViews swapChainData) !! index
                 ] | index <- Constants.swapChainImageIndices]
            , _frameBufferClearValues =
                [ getColorClearValue [0.0, 0.0, 0.2, 1.0]
                , getDepthStencilClearValue 1.0 0
                ]
            }
    | otherwise = return undefined
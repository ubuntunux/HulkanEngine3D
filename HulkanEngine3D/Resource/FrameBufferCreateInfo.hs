{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}

module HulkanEngine3D.Resource.FrameBufferCreateInfo where

import Data.IORef
import qualified Data.Text as Text

import Graphics.Vulkan

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
        let (width, height, depth) = (_imageWidth _sceneColorTexture, _imageHeight _sceneColorTexture, _imageDepth _sceneColorTexture)
        return defaultFrameBufferDataCreateInfo
            { _frameBufferName = frameBufferName
            , _frameBufferWidth = width
            , _frameBufferHeight = height
            , _frameBufferDepth = depth
            , _frameBufferSampleCount = _imageSampleCount _sceneColorTexture
            , _frameBufferViewPort = createViewport 0 0 width height 0 1
            , _frameBufferScissorRect = createScissorRect 0 0 width height
            , _frameBufferColorAttachmentFormats = [_imageFormat _sceneColorTexture]
            , _frameBufferDepthAttachmentFormats = [_imageFormat _sceneDepthTexture]
            , _frameBufferResolveAttachmentFormats = [_swapChainImageFormat swapChainData]
            , _frameBufferImageViewsList =
                [[ _imageView _sceneColorTexture
                 , _imageView _sceneDepthTexture
                 , swapChainImageView
                 ] | swapChainImageView <- _swapChainImageViews swapChainData
                ]
            , _frameBufferClearValues =
                [ getColorClearValue [0.0, 0.0, 0.2, 1.0]
                , getDepthStencilClearValue 1.0 0
                ]
            }
    | "render_final" == frameBufferName = do
        swapChainData <- readIORef (_swapChainDataRef rendererData)
        let imageSize = _swapChainExtent swapChainData
            width = getField @"width" imageSize
            height = getField @"height" imageSize
        return defaultFrameBufferDataCreateInfo
            { _frameBufferName = frameBufferName
            , _frameBufferWidth = width
            , _frameBufferHeight = height
            , _frameBufferViewPort = createViewport 0 0 width height 0 1
            , _frameBufferScissorRect = createScissorRect 0 0 width height
            , _frameBufferColorAttachmentFormats = [_swapChainImageFormat swapChainData]
            , _frameBufferImageViewsList = [_swapChainImageViews swapChainData]
            }
    | otherwise = return undefined
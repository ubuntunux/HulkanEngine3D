{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}

module HulkanEngine3D.Resource.FrameBufferCreateInfo where

import Data.IORef
import qualified Data.Text as Text

import Graphics.Vulkan

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.SwapChain


getFrameBufferDataCreateInfo :: RendererData -> Text.Text -> IO FrameBufferDataCreateInfo
getFrameBufferDataCreateInfo rendererData frameBufferName
    | "default" == frameBufferName = do
        textureSceneColor <- getRenderTarget rendererData "SceneColor"
        textureSceneDepth <- getRenderTarget rendererData "SceneDepth"
        swapChainData <- readIORef (_swapChainDataRef rendererData)
        let (width, height, depth) = (_imageWidth textureSceneColor, _imageHeight textureSceneColor, _imageDepth textureSceneColor)
        return defaultFrameBufferDataCreateInfo
            { _frameBufferName = frameBufferName
            , _frameBufferWidth = width
            , _frameBufferHeight = height
            , _frameBufferDepth = depth
            , _frameBufferSampleCount = _imageSampleCount textureSceneColor
            , _frameBufferViewPort = createViewport 0 0 width height 0 1
            , _frameBufferScissorRect = createScissorRect 0 0 width height
            , _frameBufferColorAttachmentFormats = [_imageFormat textureSceneColor]
            , _frameBufferDepthAttachmentFormats = [_imageFormat textureSceneDepth]
            , _frameBufferResolveAttachmentFormats = [_swapChainImageFormat swapChainData]
            , _frameBufferImageViewsList =
                [[ _imageView textureSceneColor
                 , _imageView textureSceneDepth
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
            , _frameBufferImageViewsList = [[swapChainImageView] | swapChainImageView <- _swapChainImageViews swapChainData]
            }
    | otherwise = return undefined
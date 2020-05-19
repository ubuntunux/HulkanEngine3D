{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}

module HulkanEngine3D.Resource.FrameBufferCreateInfo where

import Data.IORef
import qualified Data.Text as Text

import Graphics.Vulkan

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.SwapChain


getFrameBufferDataCreateInfo :: RendererData -> Text.Text -> IO FrameBufferDataCreateInfo
getFrameBufferDataCreateInfo rendererData frameBufferName
    | "render_default" == frameBufferName = do
        textureSceneAlbedo <- getRenderTarget rendererData "SceneAlbedo"
        textureSceneMaterial <- getRenderTarget rendererData "SceneMaterial"
        textureSceneNormal <- getRenderTarget rendererData "SceneNormal"
        textureSceneVelocity <- getRenderTarget rendererData "SceneVelocity"
        textureSceneDepth <- getRenderTarget rendererData "SceneDepth"
        let (width, height, depth) = (_imageWidth textureSceneAlbedo, _imageHeight textureSceneAlbedo, _imageDepth textureSceneAlbedo)
            textures = [textureSceneAlbedo, textureSceneMaterial, textureSceneNormal, textureSceneVelocity]
        return defaultFrameBufferDataCreateInfo
            { _frameBufferName = frameBufferName
            , _frameBufferWidth = width
            , _frameBufferHeight = height
            , _frameBufferDepth = depth
            , _frameBufferSampleCount = _imageSampleCount textureSceneAlbedo
            , _frameBufferViewPort = createViewport 0 0 width height 0 1
            , _frameBufferScissorRect = createScissorRect 0 0 width height
            , _frameBufferColorAttachmentFormats = [_imageFormat texture | texture <- textures]
            , _frameBufferDepthAttachmentFormats = [_imageFormat textureSceneDepth]
            , _frameBufferImageViewsList =
                replicate Constants.swapChainImageCount ([_imageView texture | texture <- textures] ++ [_imageView textureSceneDepth])
            , _frameBufferClearValues =
                [ getColorClearValue [0.0, 0.0, 0.0, 0.0]
                , getColorClearValue [0.0, 0.0, 0.0, 0.0]
                , getColorClearValue [0.5, 0.5, 1.0, 0.0]
                , getColorClearValue [0.0, 0.0, 0.0, 0.0]
                , getDepthStencilClearValue 1.0 0
                ]
            }
    | "composite_gbuffer" == frameBufferName = do
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
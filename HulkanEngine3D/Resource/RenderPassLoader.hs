{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.RenderPassLoader where

import Data.IORef
import qualified Data.Text as Text

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Vulkan
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.SwapChain


createDefaultRenderPassDataCreateInfo :: RendererData -> Text.Text -> IO RenderPassDataCreateInfo
createDefaultRenderPassDataCreateInfo rendererData renderPassName = do
    renderTargets <- readIORef (_renderTargets rendererData)
    swapChainData <- readIORef (_swapChainDataRef rendererData)
    let msaaSampleCount = (_msaaSamples . _renderFeatures $ rendererData)
        colorAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = (_imageFormat . _sceneColorTexture $ renderTargets)
                , _attachmentImageSamples = msaaSampleCount
                , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
        depthAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = (_imageFormat . _sceneDepthTexture $ renderTargets)
                , _attachmentImageSamples = msaaSampleCount
                , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_DONT_CARE
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                }
            ]
        resolveAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = (_imageFormat . _sceneColorTexture $ renderTargets)
                , _attachmentImageSamples = VK_SAMPLE_COUNT_1_BIT
                , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_DONT_CARE
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
    return RenderPassDataCreateInfo
        { _renderPassName = renderPassName
        , _vertexShaderFile = "Resource/Shaders/triangle.vert"
        , _fragmentShaderFile = "Resource/Shaders/triangle.frag"
        , _renderPassColorAttachmentDescriptions = colorAttachmentDescriptions
        , _renderPassDepthAttachmentDescriptions = depthAttachmentDescriptions
        , _renderPassResolveAttachmentDescriptions = resolveAttachmentDescriptions
        , _renderPassImageWidth = _imageWidth._sceneColorTexture $ renderTargets
        , _renderPassImageHeight = _imageHeight._sceneColorTexture $ renderTargets
        , _renderPassImageDepth = _imageDepth._sceneColorTexture $ renderTargets
        , _renderPassImageViewsList = [
            [ _imageView._sceneColorTexture $ renderTargets
            , _imageView._sceneDepthTexture $ renderTargets
            , (_swapChainImageViews swapChainData) !! index
            ] | index <- Constants.swapChainImageIndices]
        , _renderPassSampleCount = msaaSampleCount
        , _renderPassClearValues = [ getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0 ]
        }

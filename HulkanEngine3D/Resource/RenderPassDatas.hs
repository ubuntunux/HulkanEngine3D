{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassDatas where

import Data.IORef
import qualified Data.Text as Text

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTarget
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.SwapChain


getRenderPassDataCreateInfo :: RendererData -> IO RenderPassDataCreateInfo
getRenderPassDataCreateInfo rendererData = do
    renderTargets@RenderTargets {..} <- readIORef (_renderTargets rendererData)
    swapChainData <- readIORef (_swapChainDataRef rendererData)
    let renderPassName = "defaultRenderPass"::Text.Text
        sampleCount = (_msaaSamples . _renderFeatures $ rendererData)
        colorAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = _imageFormat _sceneColorTexture
                , _attachmentImageSamples = sampleCount
                , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
        depthAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = _imageFormat _sceneDepthTexture
                , _attachmentImageSamples = sampleCount
                , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_DONT_CARE
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                }
            ]
        resolveAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = _imageFormat _sceneColorTexture
                , _attachmentImageSamples = VK_SAMPLE_COUNT_1_BIT
                , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_DONT_CARE
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
        pipelineDataCreateInfo = PipelineDataCreateInfo
            { _pipelineDataCreateInfoName = "RenderTriangle"
            , _vertexShaderFile = "Resource/Shaders/triangle.vert"
            , _fragmentShaderFile = "Resource/Shaders/triangle.frag"
            , _pipelineDynamicStateList = [VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR]
            , _pipelineViewportWidth = _imageWidth _sceneColorTexture
            , _pipelineViewportHeight = _imageHeight _sceneColorTexture
            , _pipelineMultisampleCount = sampleCount
            , _pipelinePolygonMode = VK_POLYGON_MODE_FILL
            , _pipelineCullMode = VK_CULL_MODE_BACK_BIT
            , _pipelineFrontFace = VK_FRONT_FACE_CLOCKWISE
            , _pipelineColorBlendModes = [getColorBlendMode_None]
            , _depthStencilStateCreateInfo = defaultDepthStencilStateCreateInfo
            , _descriptorDataCreateInfoList =
                [ (VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, VK_SHADER_STAGE_VERTEX_BIT)
                , (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT)
                ]
            }
        viewPort = createViewport 0 0 (fromIntegral $ _imageWidth _sceneColorTexture) (fromIntegral $ _imageHeight _sceneColorTexture) 0 1
        scissorRect = createScissorRect 0 0 (fromIntegral $ _imageWidth _sceneColorTexture) (fromIntegral $ _imageHeight _sceneColorTexture)
        frameBufferDataCreateInfo = defaultFrameBufferDataCreateInfo
            { _frameBufferName = renderPassName
            , _frameBufferWidth = _imageWidth _sceneColorTexture
            , _frameBufferHeight = _imageHeight _sceneColorTexture
            , _frameBufferDepth = _imageDepth _sceneColorTexture
            , _frameBufferViewPort = viewPort
            , _frameBufferScissorRect = scissorRect
            , _frameBufferImageViewsList = [
                [ _imageView _sceneColorTexture
                , _imageView _sceneDepthTexture
                , (_swapChainImageViews swapChainData) !! index
                ] | index <- Constants.swapChainImageIndices]
            , _frameBufferClearValues = [ getColorClearValue [0.0, 0.0, 0.2, 1.0], getDepthStencilClearValue 1.0 0 ]
            }
    return RenderPassDataCreateInfo
        { _renderPassCreateInfoName = renderPassName
        , _colorAttachmentDescriptions = colorAttachmentDescriptions
        , _depthAttachmentDescriptions = depthAttachmentDescriptions
        , _resolveAttachmentDescriptions = resolveAttachmentDescriptions
        , _pipelineDataCreateInfo = pipelineDataCreateInfo
        , _frameBufferDataCreateInfo = frameBufferDataCreateInfo
        }

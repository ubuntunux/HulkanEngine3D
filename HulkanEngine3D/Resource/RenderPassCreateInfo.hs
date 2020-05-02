{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassCreateInfo where

import qualified Data.Text as Text

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Resource.FrameBufferCreateInfo
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.RenderPass


getRenderPassDataCreateInfo :: RendererData -> Text.Text -> IO RenderPassDataCreateInfo
getRenderPassDataCreateInfo rendererData renderPassName
    | "default" == renderPassName = do
        frameBufferDataCreateInfo <- getFrameBufferDataCreateInfo rendererData "default"
        let sampleCount = _frameBufferSampleCount frameBufferDataCreateInfo
            colorAttachmentDescriptions =
                [ defaultAttachmentDescription
                    { _attachmentImageFormat = (_frameBufferImageFormatList frameBufferDataCreateInfo) !! 0
                    , _attachmentImageSamples = sampleCount
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                    }
                ]
            depthAttachmentDescriptions =
                [ defaultAttachmentDescription
                    { _attachmentImageFormat = (_frameBufferImageFormatList frameBufferDataCreateInfo) !! 1
                    , _attachmentImageSamples = sampleCount
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_DONT_CARE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                    }
                ]
            resolveAttachmentDescriptions =
                [ defaultAttachmentDescription
                    { _attachmentImageFormat = (_frameBufferImageFormatList frameBufferDataCreateInfo) !! 2
                    , _attachmentImageSamples = VK_SAMPLE_COUNT_1_BIT
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_DONT_CARE
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                    }
                ]
            pipelineDataCreateInfos =
                [ PipelineDataCreateInfo
                    { _pipelineDataCreateInfoName = "RenderTriangle"
                    , _vertexShaderFile = "Resource/Shaders/triangle.vert"
                    , _fragmentShaderFile = "Resource/Shaders/triangle.frag"
                    , _pipelineDynamicStateList = [VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR]
                    , _pipelineSampleCount = sampleCount
                    , _pipelinePolygonMode = VK_POLYGON_MODE_FILL
                    , _pipelineCullMode = VK_CULL_MODE_BACK_BIT
                    , _pipelineFrontFace = VK_FRONT_FACE_CLOCKWISE
                    , _pipelineViewport = _frameBufferViewPort frameBufferDataCreateInfo
                    , _pipelineScissorRect = _frameBufferScissorRect frameBufferDataCreateInfo
                    , _pipelineColorBlendModes = [getColorBlendMode_None]
                    , _depthStencilStateCreateInfo = defaultDepthStencilStateCreateInfo
                    , _descriptorDataCreateInfoList =
                        [ (VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, VK_SHADER_STAGE_VERTEX_BIT)
                        , (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT)
                        ]
                    }
                ]
        return RenderPassDataCreateInfo
            { _renderPassCreateInfoName = renderPassName
            , _renderPassFrameBufferName = (_frameBufferName frameBufferDataCreateInfo)
            , _colorAttachmentDescriptions = colorAttachmentDescriptions
            , _depthAttachmentDescriptions = depthAttachmentDescriptions
            , _resolveAttachmentDescriptions = resolveAttachmentDescriptions
            , _pipelineDataCreateInfos = pipelineDataCreateInfos
            }
    | otherwise = return undefined
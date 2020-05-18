{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassCreateInfo where

import Data.Bits
import qualified Data.Text as Text

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Resource.FrameBufferCreateInfo
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.FrameBuffer

getRenderPassDataCreateInfo :: RendererData -> Text.Text -> IO RenderPassDataCreateInfo
getRenderPassDataCreateInfo rendererData renderPassName
    | "default" == renderPassName = do
        frameBufferDataCreateInfo <- getFrameBufferDataCreateInfo rendererData renderPassName
        let sampleCount = _frameBufferSampleCount frameBufferDataCreateInfo
            colorAttachmentDescriptions =
                [ defaultAttachmentDescription
                    { _attachmentImageFormat = format
                    , _attachmentImageSamples = sampleCount
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_GENERAL
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                    } | format <- _frameBufferColorAttachmentFormats frameBufferDataCreateInfo
                ]
            depthAttachmentDescriptions =
                [ defaultAttachmentDescription
                    { _attachmentImageFormat = format
                    , _attachmentImageSamples = sampleCount
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_DONT_CARE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                    } | format <- _frameBufferDepthAttachmentFormats frameBufferDataCreateInfo
                ]
            subpassDependencies =
                [ createSubpassDependency
                    VK_SUBPASS_EXTERNAL
                    0
                    VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
                    VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                    VK_ACCESS_MEMORY_READ_BIT
                    (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                    VK_DEPENDENCY_BY_REGION_BIT
                , createSubpassDependency
                    0
                    VK_SUBPASS_EXTERNAL
                    VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                    VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
                    (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                    VK_ACCESS_MEMORY_READ_BIT
                    VK_DEPENDENCY_BY_REGION_BIT
                ]
            pipelineDataCreateInfos =
                [ PipelineDataCreateInfo
                    { _pipelineDataCreateInfoName = "render_solid"
                    , _vertexShaderFile = "Resource/Shaders/triangle.vert"
                    , _fragmentShaderFile = "Resource/Shaders/triangle.frag"
                    , _pipelineDynamicStateList = [VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR]
                    , _pipelineSampleCount = sampleCount
                    , _pipelinePolygonMode = VK_POLYGON_MODE_FILL
                    , _pipelineCullMode = VK_CULL_MODE_BACK_BIT
                    , _pipelineFrontFace = VK_FRONT_FACE_CLOCKWISE
                    , _pipelineViewport = _frameBufferViewPort frameBufferDataCreateInfo
                    , _pipelineScissorRect = _frameBufferScissorRect frameBufferDataCreateInfo
                    , _pipelineColorBlendModes = [getColorBlendMode BlendMode_None]
                    , _depthStencilStateCreateInfo = defaultDepthStencilStateCreateInfo
                    , _descriptorDataCreateInfoList =
                        [ DescriptorDataCreateInfo "SceneConstantsData" DescriptorResourceType_UniformBuffer VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                        , DescriptorDataCreateInfo "texSampler0" DescriptorResourceType_Texture VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER VK_SHADER_STAGE_FRAGMENT_BIT
                        , DescriptorDataCreateInfo "texSampler1" DescriptorResourceType_Texture VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER VK_SHADER_STAGE_FRAGMENT_BIT
                        ]
                    }
                ]
        return RenderPassDataCreateInfo
            { _renderPassCreateInfoName = renderPassName
            , _renderPassFrameBufferName = (_frameBufferName frameBufferDataCreateInfo)
            , _colorAttachmentDescriptions = colorAttachmentDescriptions
            , _depthAttachmentDescriptions = depthAttachmentDescriptions
            , _resolveAttachmentDescriptions = []
            , _subpassDependencies = subpassDependencies
            , _pipelineDataCreateInfos = pipelineDataCreateInfos
            }
    | "render_final" == renderPassName = do
        frameBufferDataCreateInfo <- getFrameBufferDataCreateInfo rendererData renderPassName
        let sampleCount = _frameBufferSampleCount frameBufferDataCreateInfo
            colorAttachmentDescriptions =
                [ defaultAttachmentDescription
                    { _attachmentImageFormat = format
                    , _attachmentImageSamples = sampleCount
                    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_DONT_CARE
                    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                    } | format <- _frameBufferColorAttachmentFormats frameBufferDataCreateInfo
                ]
            subpassDependencies =
                [ createSubpassDependency
                    VK_SUBPASS_EXTERNAL
                    0
                    VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                    VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                    VK_ZERO_FLAGS
                    (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                    VK_DEPENDENCY_BY_REGION_BIT
                ]
            pipelineDataCreateInfos =
                [ PipelineDataCreateInfo
                    { _pipelineDataCreateInfoName = "render_quad"
                    , _vertexShaderFile = "Resource/Shaders/render_quad.vert"
                    , _fragmentShaderFile = "Resource/Shaders/render_quad.frag"
                    , _pipelineDynamicStateList = [VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR]
                    , _pipelineSampleCount = sampleCount
                    , _pipelinePolygonMode = VK_POLYGON_MODE_FILL
                    , _pipelineCullMode = VK_CULL_MODE_NONE
                    , _pipelineFrontFace = VK_FRONT_FACE_CLOCKWISE
                    , _pipelineViewport = _frameBufferViewPort frameBufferDataCreateInfo
                    , _pipelineScissorRect = _frameBufferScissorRect frameBufferDataCreateInfo
                    , _pipelineColorBlendModes = [getColorBlendMode BlendMode_None]
                    , _depthStencilStateCreateInfo = defaultDepthStencilStateCreateInfo  { _depthWriteEnable = VK_FALSE }
                    , _descriptorDataCreateInfoList =
                        [ DescriptorDataCreateInfo "SceneConstantsData" DescriptorResourceType_UniformBuffer VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                        , DescriptorDataCreateInfo "texSampler" DescriptorResourceType_Texture VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER VK_SHADER_STAGE_FRAGMENT_BIT
                        , DescriptorDataCreateInfo "SceneColor" DescriptorResourceType_RenderTarget VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER VK_SHADER_STAGE_FRAGMENT_BIT
                        ]
                    }
                ]
        return RenderPassDataCreateInfo
            { _renderPassCreateInfoName = renderPassName
            , _renderPassFrameBufferName = (_frameBufferName frameBufferDataCreateInfo)
            , _colorAttachmentDescriptions = colorAttachmentDescriptions
            , _depthAttachmentDescriptions = []
            , _resolveAttachmentDescriptions = []
            , _subpassDependencies = subpassDependencies
            , _pipelineDataCreateInfos = pipelineDataCreateInfos
            }
    | otherwise = return undefined
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassCreateInfo.RenderShadow where

import Data.Bits
import qualified Data.Text as Text

import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTargetDeclaration
import HulkanEngine3D.Render.UniformBufferDatas (UniformBufferType (..))
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Utilities.System (toText)


renderPassName :: Text.Text
renderPassName = "render_shadow"

getFrameBufferDataCreateInfo :: RendererData -> IO FrameBufferDataCreateInfo
getFrameBufferDataCreateInfo rendererData = do
    textureSceneDepth <- getRenderTarget rendererData RenderTarget_SceneDepth
    let (width, height, depth) = (_imageWidth textureSceneDepth, _imageHeight textureSceneDepth, _imageDepth textureSceneDepth)
    return defaultFrameBufferDataCreateInfo
        { _frameBufferName = renderPassName
        , _frameBufferWidth = width
        , _frameBufferHeight = height
        , _frameBufferDepth = depth
        , _frameBufferSampleCount = _imageSampleCount textureSceneDepth
        , _frameBufferViewPort = createViewport 0 0 width height 0 1
        , _frameBufferScissorRect = createScissorRect 0 0 width height
        , _frameBufferColorAttachmentFormats = []
        , _frameBufferDepthAttachmentFormats = [_imageFormat textureSceneDepth]
        , _frameBufferImageViewLists = swapChainIndexMapSingleton [ _imageView textureSceneDepth ]
        , _frameBufferClearValues = [ getDepthStencilClearValue 1.0 0 ]
        }

getRenderPassDataCreateInfo :: RendererData -> IO RenderPassDataCreateInfo
getRenderPassDataCreateInfo rendererData = do
    frameBufferDataCreateInfo <- getFrameBufferDataCreateInfo rendererData
    let sampleCount = _frameBufferSampleCount frameBufferDataCreateInfo
        colorAttachmentDescriptions = []
        depthAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = format
                , _attachmentImageSamples = sampleCount
                , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_CLEAR
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_GENERAL
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
                { _pipelineDataCreateInfoName = "render_shadow"
                , _pipelineVertexShaderFile = "render_object.vert"
                , _pipelineFragmentShaderFile = "shadowmap.frag"
                , _pipelineShaderDefines = [Text.append "RenderMode=" (Text.pack . show . fromEnum $ Constants.RenderMode_Shadow)]
                , _pipelineDynamicStateList = [VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR]
                , _pipelineSampleCount = sampleCount
                , _pipelinePolygonMode = VK_POLYGON_MODE_FILL
                , _pipelineCullMode = VK_CULL_MODE_BACK_BIT
                , _pipelineFrontFace = VK_FRONT_FACE_CLOCKWISE
                , _pipelineViewport = _frameBufferViewPort frameBufferDataCreateInfo
                , _pipelineScissorRect = _frameBufferScissorRect frameBufferDataCreateInfo
                , _pipelineColorBlendModes = replicate (length colorAttachmentDescriptions) $ getColorBlendMode BlendMode_None
                , _depthStencilStateCreateInfo = defaultDepthStencilStateCreateInfo
                , _descriptorDataCreateInfoList =
                    [ DescriptorDataCreateInfo
                        0
                        (toText UniformBuffer_SceneConstants)
                        DescriptorResourceType_UniformBuffer
                        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                        (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                    , DescriptorDataCreateInfo
                        1
                        (toText UniformBuffer_ViewConstants)
                        DescriptorResourceType_UniformBuffer
                        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                        (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                    , DescriptorDataCreateInfo
                        2
                        (toText UniformBuffer_LightConstants)
                        DescriptorResourceType_UniformBuffer
                        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                        (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                    , DescriptorDataCreateInfo
                        3
                        "textureBase"
                        DescriptorResourceType_Texture
                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                        VK_SHADER_STAGE_FRAGMENT_BIT
                    ]
                }
            ]
    return RenderPassDataCreateInfo
        { _renderPassCreateInfoName = renderPassName
        , _renderPassFrameBufferCreateInfo = frameBufferDataCreateInfo
        , _colorAttachmentDescriptions = colorAttachmentDescriptions
        , _depthAttachmentDescriptions = depthAttachmentDescriptions
        , _resolveAttachmentDescriptions = []
        , _subpassDependencies = subpassDependencies
        , _pipelineDataCreateInfos = pipelineDataCreateInfos
        }
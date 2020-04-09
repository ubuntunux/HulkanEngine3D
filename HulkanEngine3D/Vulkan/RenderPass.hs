{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE TypeApplications       #-}

module HulkanEngine3D.Vulkan.RenderPass
    ( ImageAttachmentDescription (..)
    , RenderPassDataCreateInfo (..)
    , PipelineDataCreateInfo (..)
    , DepthStencilStateCreateInfo (..)
    , GraphicsPipelineData (..)
    , RenderPassData (..)
    , defaultDepthStencilStateCreateInfo
    , defaultAttachmentDescription
    , createRenderPassData
    , destroyRenderPassData
    , createGraphicsPipeline
    , destroyGraphicsPipeline
    ) where

import Data.Bits
import qualified Data.Text as Text
import Foreign.Marshal.Alloc
import Foreign.Storable

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame
import Numeric.Dimensions

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.GeometryBuffer
import HulkanEngine3D.Vulkan.PushConstant
import HulkanEngine3D.Vulkan.Shader


data ImageAttachmentDescription = ImageAttachmentDescription
    { _attachmentImageFormat :: VkFormat
    , _attachmentImageSamples :: VkSampleCountFlagBits
    , _attachmentLoadOperation :: VkAttachmentLoadOp
    , _attachmentStoreOperation :: VkAttachmentStoreOp
    , _attachmentStencilLoadOperation :: VkAttachmentLoadOp
    , _attachmentStencilStoreOperation :: VkAttachmentStoreOp
    , _attachmentInitialLayout :: VkImageLayout
    , _attachmentFinalLayout :: VkImageLayout
    , _attachmentReferenceLayout :: VkImageLayout
    } deriving (Eq, Show)

data RenderPassDataCreateInfo = RenderPassDataCreateInfo
    { _renderPassCreateInfoName :: Text.Text
    , _colorAttachmentDescriptions :: [ImageAttachmentDescription]
    , _depthAttachmentDescriptions :: [ImageAttachmentDescription]
    , _resolveAttachmentDescriptions :: [ImageAttachmentDescription]
    , _pipelineDataCreateInfo :: PipelineDataCreateInfo
    , _frameBufferDataCreateInfo :: FrameBufferDataCreateInfo
    }  deriving (Eq, Show)

data DepthStencilStateCreateInfo = DepthStencilStateCreateInfo
    { _depthTestEnable :: VkBool32
    , _depthWriteEnable :: VkBool32
    , _depthCompareOp :: VkCompareOp
    , _stencilTestEnable :: VkBool32
    , _frontFailOp :: VkStencilOp -- fail the stencil test
    , _frontPassOp :: VkStencilOp -- pass both the depth and stencil tests
    , _frontDepthFailOp :: VkStencilOp -- pass the stencil test and fail the depth test
    , _frontCompareOp :: VkCompareOp
    , _frontCompareMask :: Word32
    , _frontWriteMask :: Word32
    , _frontReference :: Word32
    , _backFailOp :: VkStencilOp -- fail the stencil test
    , _backPassOp :: VkStencilOp -- pass both the depth and stencil tests
    , _backDepthFailOp :: VkStencilOp -- pass the stencil test and fail the depth test
    , _backCompareOp :: VkCompareOp
    , _backCompareMask :: Word32
    , _backWriteMask :: Word32
    , _backReference :: Word32
    } deriving (Eq, Show)

data PipelineDataCreateInfo = PipelineDataCreateInfo
    { _pipelineDataCreateInfoName :: Text.Text
    , _vertexShaderFile :: String
    , _fragmentShaderFile :: String
    , _pipelineDynamicStateList :: [VkDynamicState]
    , _pipelineViewportWidth :: Int
    , _pipelineViewportHeight :: Int
    , _pipelineMultisampleCount :: VkSampleCountFlagBits
    , _pipelinePolygonMode :: VkPolygonMode
    , _pipelineCullMode :: VkCullModeFlagBits
    , _pipelineFrontFace :: VkFrontFace
    , _pipelineColorBlendModes :: [VkPipelineColorBlendAttachmentState]
    , _depthStencilStateCreateInfo :: DepthStencilStateCreateInfo
    , _descriptorDataCreateInfoList :: [DescriptorDataCreateInfo]
    }  deriving (Eq, Show)

data GraphicsPipelineData = GraphicsPipelineData
    { _pipelineDataName :: Text.Text
    , _vertexShaderCreateInfo :: VkPipelineShaderStageCreateInfo
    , _fragmentShaderCreateInfo :: VkPipelineShaderStageCreateInfo
    , _pipelineLayout :: VkPipelineLayout
    , _pipeline :: VkPipeline
    , _descriptorData :: DescriptorData
    } deriving (Eq, Show)

data RenderPassData = RenderPassData
    { _renderPassDataName :: Text.Text
    , _renderPass :: VkRenderPass
    , _graphicsPipelineData :: GraphicsPipelineData
    } deriving (Eq, Show)


defaultDepthStencilStateCreateInfo :: DepthStencilStateCreateInfo
defaultDepthStencilStateCreateInfo = DepthStencilStateCreateInfo
   { _depthTestEnable = VK_TRUE
   , _depthWriteEnable = VK_TRUE
   , _depthCompareOp = VK_COMPARE_OP_LESS
   , _stencilTestEnable = VK_FALSE
   , _frontFailOp = VK_STENCIL_OP_KEEP
   , _frontPassOp = VK_STENCIL_OP_KEEP
   , _frontDepthFailOp = VK_STENCIL_OP_KEEP
   , _frontCompareOp = VK_COMPARE_OP_NEVER
   , _frontCompareMask = 0
   , _frontWriteMask = 0
   , _frontReference = 0
   , _backFailOp = VK_STENCIL_OP_KEEP
   , _backPassOp = VK_STENCIL_OP_KEEP
   , _backDepthFailOp = VK_STENCIL_OP_KEEP
   , _backCompareOp = VK_COMPARE_OP_NEVER
   , _backCompareMask = 0
   , _backWriteMask = 0
   , _backReference = 0
   }

defaultAttachmentDescription :: ImageAttachmentDescription
defaultAttachmentDescription = ImageAttachmentDescription
    { _attachmentImageFormat = VK_FORMAT_UNDEFINED
    , _attachmentImageSamples = VK_SAMPLE_COUNT_1_BIT
    , _attachmentLoadOperation = VK_ATTACHMENT_LOAD_OP_DONT_CARE
    , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_DONT_CARE
    , _attachmentStencilLoadOperation = VK_ATTACHMENT_LOAD_OP_DONT_CARE
    , _attachmentStencilStoreOperation = VK_ATTACHMENT_STORE_OP_DONT_CARE
    , _attachmentInitialLayout = VK_IMAGE_LAYOUT_UNDEFINED
    , _attachmentFinalLayout = VK_IMAGE_LAYOUT_UNDEFINED
    , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_UNDEFINED
    }

createRenderPassData :: VkDevice -> RenderPassDataCreateInfo -> DescriptorData -> IO RenderPassData
createRenderPassData device renderPassDataCreateInfo@RenderPassDataCreateInfo {..} descriptorData = do
    renderPass <- createRenderPass device renderPassDataCreateInfo
    graphicsPipelineData <- createGraphicsPipeline device renderPass _pipelineDataCreateInfo descriptorData
    logInfo $ "CreateRenderPassData : " ++ (Text.unpack _renderPassCreateInfoName)
    return RenderPassData
        { _renderPassDataName = _renderPassCreateInfoName
        , _renderPass = renderPass
        , _graphicsPipelineData = graphicsPipelineData
        }

destroyRenderPassData :: VkDevice -> RenderPassData -> IO ()
destroyRenderPassData device renderPassData@RenderPassData {..} = do
    logInfo "DestroyRenderPassData"
    destroyGraphicsPipeline device _graphicsPipelineData
    destroyRenderPass device _renderPass _renderPassDataName


createRenderPass :: VkDevice -> RenderPassDataCreateInfo -> IO VkRenderPass
createRenderPass device renderPassDataCreateInfo@RenderPassDataCreateInfo {..} = do
    let imageAttachment :: ImageAttachmentDescription -> VkAttachmentDescription
        imageAttachment attachmentDescription = createVk @VkAttachmentDescription
            $  set @"flags" VK_ZERO_FLAGS
            &* set @"format" (_attachmentImageFormat attachmentDescription)
            &* set @"samples" (_attachmentImageSamples attachmentDescription)
            &* set @"loadOp" (_attachmentLoadOperation attachmentDescription)
            &* set @"storeOp" (_attachmentStoreOperation attachmentDescription)
            &* set @"stencilLoadOp" (_attachmentStencilLoadOperation attachmentDescription)
            &* set @"stencilStoreOp" (_attachmentStencilStoreOperation attachmentDescription)
            &* set @"initialLayout" (_attachmentInitialLayout attachmentDescription)
            &* set @"finalLayout" (_attachmentFinalLayout attachmentDescription)
        imageAttachmentReference :: ImageAttachmentDescription -> Int -> VkAttachmentReference
        imageAttachmentReference attachmentDescription index = createVk @VkAttachmentReference
            $  set @"attachment" (fromIntegral index)
            &* set @"layout" (_attachmentReferenceLayout attachmentDescription)
        attachmentDescriptions = _colorAttachmentDescriptions ++ _depthAttachmentDescriptions ++ _resolveAttachmentDescriptions
        imageAttachments = map imageAttachment attachmentDescriptions
        colorAttachmentCount = length _colorAttachmentDescriptions
        depthAttachmentCount = length _depthAttachmentDescriptions
        resolveAttachmentCount = length _resolveAttachmentDescriptions
        colorAttachmentReferences = zipWith imageAttachmentReference _colorAttachmentDescriptions [0..]
        depthAttachmentReferences = zipWith imageAttachmentReference _depthAttachmentDescriptions [colorAttachmentCount..]
        resolveAttachmentReferences = zipWith imageAttachmentReference _resolveAttachmentDescriptions [(colorAttachmentCount + depthAttachmentCount)..]
        subpass = createVk @VkSubpassDescription
            $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
            &* set @"colorAttachmentCount" (fromIntegral . length $ colorAttachmentReferences)
            &* case colorAttachmentReferences of
                [] -> set @"pColorAttachments" VK_NULL
                otherwise -> setListRef @"pColorAttachments" colorAttachmentReferences
            &* case depthAttachmentReferences of
                [] -> set @"pDepthStencilAttachment" VK_NULL
                otherwise -> setListRef @"pDepthStencilAttachment" depthAttachmentReferences
            &* case resolveAttachmentReferences of
                [] -> set @"pResolveAttachments" VK_NULL
                otherwise -> setListRef @"pResolveAttachments" resolveAttachmentReferences
            &* set @"pPreserveAttachments" VK_NULL
            &* set @"pInputAttachments" VK_NULL
        dependency = createVk @VkSubpassDependency
            $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
            &* set @"dstSubpass" 0
            &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            &* set @"srcAccessMask" VK_ZERO_FLAGS
            &* set @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
            &* set @"dependencyFlags" VK_DEPENDENCY_BY_REGION_BIT
        renderPassCreateInfo = createVk @VkRenderPassCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"attachmentCount" (fromIntegral . length $ imageAttachments)
            &* setListRef @"pAttachments" imageAttachments
            &* set @"subpassCount" 1
            &* setVkRef @"pSubpasses" subpass
            &* set @"dependencyCount" 1
            &* setVkRef @"pDependencies" dependency
    renderPass <- alloca $ \renderPassPtr -> do
        result <- vkCreateRenderPass device (unsafePtr renderPassCreateInfo) VK_NULL renderPassPtr
        validationVK result "vkCreatePipelineLayout failed!"
        peek renderPassPtr
    touchVkData renderPassCreateInfo
    logInfo $ "Create RenderPass: " ++ show (fmap _attachmentImageFormat attachmentDescriptions)
    return renderPass


destroyRenderPass :: VkDevice -> VkRenderPass -> Text.Text -> IO ()
destroyRenderPass device renderPass renderPassName = do
    logInfo $ "Destroy RenderPass : " ++ Text.unpack renderPassName ++ " " ++ show renderPass
    vkDestroyRenderPass device renderPass VK_NULL


createPipelineLayout :: VkDevice -> [VkPushConstantRange] -> [VkDescriptorSetLayout] -> IO VkPipelineLayout
createPipelineLayout device pushConstantRanges descriptorSetLayouts = do
    let pipelineCreateInfo = createVk @VkPipelineLayoutCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"setLayoutCount" (fromIntegral . length $ descriptorSetLayouts)
            &* setListRef @"pSetLayouts" descriptorSetLayouts
            &* set @"pushConstantRangeCount" (fromIntegral . length $ pushConstantRanges)
            &* setListRef @"pPushConstantRanges" pushConstantRanges
    pipelineLayout <- alloca $ \pipelineLayoutPtr -> do
        result <- vkCreatePipelineLayout device (unsafePtr pipelineCreateInfo) VK_NULL pipelineLayoutPtr
        validationVK result "vkCreatePipelineLayout failed!"
        peek pipelineLayoutPtr
    touchVkData pipelineCreateInfo
    logInfo $ "Create PipelineLayout : " ++ (show pipelineLayout)
    return pipelineLayout


destroyPipelineLayout :: VkDevice -> VkPipelineLayout -> IO ()
destroyPipelineLayout device pipelineLayout = do
    logInfo $ "Destroy PipelineLayout" ++ show pipelineLayout
    vkDestroyPipelineLayout device pipelineLayout VK_NULL


createGraphicsPipeline :: VkDevice
                       -> VkRenderPass
                       -> PipelineDataCreateInfo
                       -> DescriptorData
                       -> IO GraphicsPipelineData
createGraphicsPipeline device renderPass pipelineDataCreateInfo@PipelineDataCreateInfo {..} descriptorData = do
    vertexShaderCreateInfo <- createShaderStageCreateInfo device _vertexShaderFile VK_SHADER_STAGE_VERTEX_BIT
    fragmentShaderCreateInfo <- createShaderStageCreateInfo device _fragmentShaderFile VK_SHADER_STAGE_FRAGMENT_BIT

    let depthStencilStateCreateInfo@DepthStencilStateCreateInfo {..} = _depthStencilStateCreateInfo
        pushConstantData = PushConstantData { modelMatrix = matrix4x4_indentity }
        pushConstantRange = getPushConstantRange pushConstantData
        shaderStageInfos = [vertexShaderCreateInfo, fragmentShaderCreateInfo]
        shaderStageInfoCount = length shaderStageInfos
        descriptorSetCount = Constants.swapChainImageCount

    pipelineLayout <- createPipelineLayout device [pushConstantRange] [_descriptorSetLayout descriptorData]

    let vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"vertexBindingDescriptionCount" 1
            &* setDFRef @"pVertexBindingDescriptions" (scalar vertexInputBindDescription)
            &* set @"vertexAttributeDescriptionCount" (fromIntegral . totalDim $ dims `inSpaceOf` vertexInputAttributeDescriptions)
            &* setDFRef @"pVertexAttributeDescriptions" vertexInputAttributeDescriptions
        inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
            &* set @"primitiveRestartEnable" VK_FALSE
        dynamicState = createVk @VkPipelineDynamicStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"dynamicStateCount" (fromIntegral . length $ _pipelineDynamicStateList)
            &* setListRef @"pDynamicStates" _pipelineDynamicStateList
        viewPort = createVk @VkViewport
            $  set @"x" 0
            &* set @"y" 0
            &* set @"width" (fromIntegral _pipelineViewportWidth)
            &* set @"height" (fromIntegral _pipelineViewportHeight)
            &* set @"minDepth" 0
            &* set @"maxDepth" 1
        scissorRect = createVk @VkRect2D
            $  setVk @"extent"
                (  set @"width" (fromIntegral _pipelineViewportWidth)
                &* set @"height" (fromIntegral _pipelineViewportHeight) )
            &* setVk @"offset"
                (  set @"x" 0
                &* set @"y" 0 )
        viewPortState = createVk @VkPipelineViewportStateCreateInfo
            $ set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"viewportCount" 1
            &* setVkRef @"pViewports" viewPort
            &* set @"scissorCount" 1
            &* setVkRef @"pScissors" scissorRect
        rasterizer = createVk @VkPipelineRasterizationStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"depthClampEnable" VK_FALSE
            &* set @"rasterizerDiscardEnable" VK_FALSE
            &* set @"polygonMode" _pipelinePolygonMode
            &* set @"cullMode" (bitToMask _pipelineCullMode)
            &* set @"frontFace" _pipelineFrontFace
            &* set @"depthBiasEnable" VK_FALSE
            &* set @"depthBiasConstantFactor" 0
            &* set @"depthBiasClamp" 0
            &* set @"depthBiasSlopeFactor" 0
            &* set @"lineWidth" 1.0
        multisampling = createVk @VkPipelineMultisampleStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"sampleShadingEnable" VK_FALSE
            &* set @"rasterizationSamples" _pipelineMultisampleCount
            &* set @"minSampleShading" 1.0
            &* set @"pSampleMask" VK_NULL
            &* set @"alphaToCoverageEnable" VK_FALSE
            &* set @"alphaToOneEnable" VK_FALSE
        colorBlending = createVk @VkPipelineColorBlendStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"logicOpEnable" VK_FALSE
            &* set @"logicOp" VK_LOGIC_OP_COPY
            &* set @"attachmentCount" (fromIntegral . length $ _pipelineColorBlendModes)
            &* setListRef @"pAttachments" _pipelineColorBlendModes
            &* setAt @"blendConstants" @0 0.0
            &* setAt @"blendConstants" @1 0.0
            &* setAt @"blendConstants" @2 0.0
            &* setAt @"blendConstants" @3 0.0
        depthStencilState = createVk @VkPipelineDepthStencilStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"depthTestEnable" _depthTestEnable
            &* set @"depthWriteEnable" _depthWriteEnable
            &* set @"depthCompareOp" _depthCompareOp
            &* set @"depthBoundsTestEnable" VK_FALSE
            &* set @"minDepthBounds" 0.0
            &* set @"maxDepthBounds" 1.0
            &* set @"stencilTestEnable" _stencilTestEnable
            &* setVk @"front"
                (  set @"failOp" _frontFailOp
                &* set @"passOp" _frontPassOp
                &* set @"depthFailOp" _frontDepthFailOp
                &* set @"compareOp" _frontCompareOp
                &* set @"compareMask" _frontCompareMask
                &* set @"writeMask" _frontWriteMask
                &* set @"reference" _frontReference )
            &* setVk @"back"
                (  set @"failOp" _backFailOp
                &* set @"passOp" _backPassOp
                &* set @"depthFailOp" _backDepthFailOp
                &* set @"compareOp" _backCompareOp
                &* set @"compareMask" _backCompareMask
                &* set @"writeMask" _backWriteMask
                &* set @"reference" _backReference )
        graphicsPipelineCreateInfo = createVk @VkGraphicsPipelineCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"stageCount" (fromIntegral shaderStageInfoCount)
            &* setListRef @"pStages" shaderStageInfos
            &* setVkRef @"pVertexInputState" vertexInputInfo
            &* setVkRef @"pInputAssemblyState" inputAssembly
            &* set @"pTessellationState" VK_NULL
            &* setVkRef @"pViewportState" viewPortState
            &* setVkRef @"pRasterizationState" rasterizer
            &* setVkRef @"pMultisampleState" multisampling
            &* setVkRef @"pDepthStencilState" depthStencilState
            &* setVkRef @"pColorBlendState" colorBlending
            &* setVkRef @"pDynamicState" dynamicState
            &* set @"layout" pipelineLayout
            &* set @"renderPass" renderPass
            &* set @"subpass" 0
            &* set @"basePipelineHandle" VK_NULL_HANDLE
            &* set @"basePipelineIndex" (-1)

    logInfo $ "createGraphicsPipeline"
    logInfo $ "    vertexShader : " ++ show _vertexShaderFile
    logInfo $ "    fragmentShader : " ++ show _fragmentShaderFile
    graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
        withPtr graphicsPipelineCreateInfo $ \graphicsPipelineCreateInfoPtr -> do
            result <- vkCreateGraphicsPipelines device VK_NULL_HANDLE 1 graphicsPipelineCreateInfoPtr VK_NULL graphicsPipelinePtr
            validationVK result "vkCreateGraphicsPipelines failed!"
        peek graphicsPipelinePtr

    return GraphicsPipelineData
        { _pipelineDataName = _pipelineDataCreateInfoName
        , _vertexShaderCreateInfo = vertexShaderCreateInfo
        , _fragmentShaderCreateInfo = fragmentShaderCreateInfo
        , _pipeline = graphicsPipeline
        , _pipelineLayout = pipelineLayout
        , _descriptorData = descriptorData
        }


destroyGraphicsPipeline :: VkDevice -> GraphicsPipelineData -> IO ()
destroyGraphicsPipeline device graphicsPipelineData@GraphicsPipelineData {..} = do
    logInfo $ "Destroy GraphicsPipeline : " ++ Text.unpack _pipelineDataName ++ "pipeline " ++ show _pipeline ++ ", pipelineLayout" ++ show _pipelineLayout
    vkDestroyPipeline device _pipeline VK_NULL
    destroyPipelineLayout device _pipelineLayout
    destroyShaderStageCreateInfo device _vertexShaderCreateInfo
    destroyShaderStageCreateInfo device _fragmentShaderCreateInfo

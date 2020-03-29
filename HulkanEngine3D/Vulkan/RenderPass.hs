{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.RenderPass
    ( GraphicsPipelineData (..)
    , RenderPassData (..)
    , RenderPassDataCreateInfo (..)
    , RenderPassInterface (..)
    , createRenderPassData
    , destroyRenderPassData
    , createGraphicsPipeline
    , destroyGraphicsPipeline
    ) where

import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame
import Numeric.Dimensions

import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.GeometryBuffer
import HulkanEngine3D.Vulkan.PushConstant
import HulkanEngine3D.Vulkan.Shader


data GraphicsPipelineData = GraphicsPipelineData
    { _vertexShaderCreateInfo :: VkPipelineShaderStageCreateInfo
    , _fragmentShaderCreateInfo :: VkPipelineShaderStageCreateInfo
    , _descriptorSetLayout :: VkDescriptorSetLayout
    , _pipelineLayout :: VkPipelineLayout
    , _pipeline :: VkPipeline
    } deriving (Eq, Show)

data RenderPassData = RenderPassData
    { _graphicsPipelineData :: GraphicsPipelineData
    , _renderPass :: VkRenderPass
    , _frameBufferData :: FrameBufferData
    } deriving (Eq, Show)


data RenderPassDataCreateInfo = RenderPassDataCreateInfo
    { _vertexShaderFile :: String
    , _fragmentShaderFile :: String
    , _renderPassSwapChainImageCount :: Int
    , _renderPassImageFormat :: [VkFormat]
    , _renderPassImageWidth :: Int
    , _renderPassImageHeight :: Int
    , _renderPassImageDepth :: Int
    , _renderPassSampleCount :: VkSampleCountFlagBits
    , _renderPassImageViews :: [VkImageView]
    , _renderPassResolveImageViews :: [VkImageView]
    , _renderPassClearValues :: [VkClearValue]
    }  deriving (Eq, Show)


class RenderPassInterface a where
    getDescriptorSetLayout :: a -> VkDescriptorSetLayout

instance RenderPassInterface RenderPassData where
    getDescriptorSetLayout renderPassData = (_descriptorSetLayout (_graphicsPipelineData renderPassData))


createRenderPassData :: VkDevice -> RenderPassDataCreateInfo -> IO RenderPassData
createRenderPassData device renderPassDataCreateInfo@RenderPassDataCreateInfo {..} = do
    renderPass <- createRenderPass device _renderPassImageFormat _renderPassDepthFormat _renderPassSampleCount
    graphicsPipelineData <- createGraphicsPipeline device renderPass _renderPassImageExtent _renderPassSampleCount _vertexShaderFile _fragmentShaderFile
    frameBufferData <- createFramebufferData device renderPass _renderPassSwapChainImageCount _renderPassImageViews _renderPassResolveImageViews _renderPassImageExtent _renderPassSampleCount _renderPassClearValues
    let renderPassData = RenderPassData
            { _renderPass = renderPass
            , _graphicsPipelineData = graphicsPipelineData
            , _frameBufferData = frameBufferData }
    logInfo "CreateRenderPassData"
    return renderPassData

destroyRenderPassData :: VkDevice -> RenderPassData -> IO ()
destroyRenderPassData device renderPassData@RenderPassData {..} = do
    logInfo "DestroyRenderPassData"
    destroyFramebufferData device _frameBufferData
    destroyGraphicsPipeline device _graphicsPipelineData
    destroyRenderPass device _renderPass

createRenderPass :: VkDevice -> VkFormat -> VkFormat -> VkSampleCountFlagBits -> IO VkRenderPass
createRenderPass device imageFormat depthFormat samples = do
    let isMSAA = (samples /= VK_SAMPLE_COUNT_1_BIT)
        colorAttachment = createVk @VkAttachmentDescription
            $  set @"flags" VK_ZERO_FLAGS
            &* set @"format" imageFormat
            &* set @"samples" samples
            &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
            &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
            &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
            &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
            &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
            &* set @"finalLayout" (if isMSAA
                then VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                else VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
        depthAttachment = createVk @VkAttachmentDescription
            $  set @"flags" VK_ZERO_FLAGS
            &* set @"format" depthFormat
            &* set @"samples" samples
            &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
            &* set @"storeOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
            &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
            &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
            &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
            &* set @"finalLayout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        colorAttachmentResolve = createVk @VkAttachmentDescription
            $  set @"flags" VK_ZERO_FLAGS
            &* set @"format" imageFormat
            &* set @"samples" VK_SAMPLE_COUNT_1_BIT
            &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
            &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
            &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
            &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
            &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
            &* set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
        colorAttachmentRef = createVk @VkAttachmentReference
            $  set @"attachment" 0
            &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        depthAttachmentRef = createVk @VkAttachmentReference
            $  set @"attachment" 1
            &* set @"layout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        colorAttachmentResolveRef = createVk @VkAttachmentReference
            $  set @"attachment" 2
            &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        subpass = createVk @VkSubpassDescription
            $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
            &* set @"colorAttachmentCount" 1
            &* setVkRef @"pColorAttachments" colorAttachmentRef
            &* setVkRef @"pDepthStencilAttachment" depthAttachmentRef
            &* (if isMSAA
                    then setVkRef @"pResolveAttachments" colorAttachmentResolveRef
                    else set @"pResolveAttachments" VK_NULL)
            &* set @"pPreserveAttachments" VK_NULL
            &* set @"pInputAttachments" VK_NULL
        dependency = createVk @VkSubpassDependency
            $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
            &* set @"dstSubpass" 0
            &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            &* set @"srcAccessMask" VK_ZERO_FLAGS
            &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            &* set @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
        attachments = if isMSAA
            then [colorAttachment, depthAttachment, colorAttachmentResolve]
            else [colorAttachment, depthAttachment]
        renderPassCreateInfo = createVk @VkRenderPassCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"attachmentCount" (fromIntegral $ length attachments)
            &* setListRef @"pAttachments" attachments
            &* set @"subpassCount" 1
            &* setVkRef @"pSubpasses" subpass
            &* set @"dependencyCount" 1
            &* setVkRef @"pDependencies" dependency

    renderPass <- alloca $ \renderPassPtr -> do
        result <- vkCreateRenderPass device (unsafePtr renderPassCreateInfo) VK_NULL renderPassPtr
        validationVK result "vkCreatePipelineLayout failed!"
        peek renderPassPtr
    touchVkData renderPassCreateInfo
    logInfo $ "Create RenderPass: " ++ show imageFormat
    return renderPass


destroyRenderPass :: VkDevice -> VkRenderPass -> IO ()
destroyRenderPass device renderPass = do
    logInfo $ "Destroy RenderPass : " ++ show renderPass
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
                       -> VkExtent2D
                       -> VkSampleCountFlagBits
                       -> String
                       -> String
                       -> IO GraphicsPipelineData
createGraphicsPipeline device renderPass imageExtent msaaSamples vertexShaderFile fragmentShaderFile = do
    vertexShaderCreateInfo <- createShaderStageCreateInfo device vertexShaderFile VK_SHADER_STAGE_VERTEX_BIT
    fragmentShaderCreateInfo <- createShaderStageCreateInfo device fragmentShaderFile VK_SHADER_STAGE_FRAGMENT_BIT
    descriptorSetLayout <- createDescriptorSetLayout device
    let pushConstantData = PushConstantData { modelMatrix = matrix4x4_indentity }
        pushConstantRange = getPushConstantRange pushConstantData
    pipelineLayout <- createPipelineLayout device [pushConstantRange] [descriptorSetLayout]

    let shaderStageInfos = [vertexShaderCreateInfo, fragmentShaderCreateInfo]
        shaderStageInfoCount = length shaderStageInfos

    let vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"vertexBindingDescriptionCount" 1
            &* setDFRef @"pVertexBindingDescriptions" (scalar vertexInputBindDescription)
            &* set @"vertexAttributeDescriptionCount"
                (fromIntegral . totalDim $ dims `inSpaceOf` vertexInputAttributeDescriptions)
            &* setDFRef @"pVertexAttributeDescriptions" vertexInputAttributeDescriptions

        inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
            &* set @"primitiveRestartEnable" VK_FALSE

        viewPort = createVk @VkViewport
            $  set @"x" 0
            &* set @"y" 0
            &* set @"width" (fromIntegral $ getField @"width" imageExtent)
            &* set @"height" (fromIntegral $ getField @"height" imageExtent)
            &* set @"minDepth" 0
            &* set @"maxDepth" 1

        scissorRect = createVk @VkRect2D
            $  set   @"extent" imageExtent
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
            &* set @"polygonMode" VK_POLYGON_MODE_FILL
            &* set @"cullMode" VK_CULL_MODE_BACK_BIT
            &* set @"frontFace" VK_FRONT_FACE_CLOCKWISE
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
            &* set @"rasterizationSamples" msaaSamples
            &* set @"minSampleShading" 1.0
            &* set @"pSampleMask" VK_NULL
            &* set @"alphaToCoverageEnable" VK_FALSE
            &* set @"alphaToOneEnable" VK_FALSE

        colorBlendAttachment = createVk @VkPipelineColorBlendAttachmentState
            $  set @"colorWriteMask"
                (   VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
                .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT )
            &* set @"blendEnable" VK_FALSE
            &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE
            &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO
            &* set @"colorBlendOp" VK_BLEND_OP_ADD
            &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
            &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
            &* set @"alphaBlendOp" VK_BLEND_OP_ADD

        colorBlending = createVk @VkPipelineColorBlendStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"logicOpEnable" VK_FALSE
            &* set @"logicOp" VK_LOGIC_OP_COPY
            &* set @"attachmentCount" 1
            &* setVkRef @"pAttachments" colorBlendAttachment
            &* setAt @"blendConstants" @0 0.0
            &* setAt @"blendConstants" @1 0.0
            &* setAt @"blendConstants" @2 0.0
            &* setAt @"blendConstants" @3 0.0

        depthStencilState = createVk @VkPipelineDepthStencilStateCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"depthTestEnable" VK_TRUE
            &* set @"depthWriteEnable" VK_TRUE
            &* set @"depthCompareOp" VK_COMPARE_OP_LESS
            &* set @"depthBoundsTestEnable" VK_FALSE
            &* set @"minDepthBounds" 0.0
            &* set @"maxDepthBounds" 1.0
            &* set @"stencilTestEnable" VK_FALSE
            &* setVk @"front"
                (  set @"failOp" VK_STENCIL_OP_KEEP
                &* set @"passOp" VK_STENCIL_OP_KEEP
                &* set @"depthFailOp" VK_STENCIL_OP_KEEP
                &* set @"compareOp" VK_COMPARE_OP_NEVER
                &* set @"compareMask" 0
                &* set @"writeMask" 0
                &* set @"reference" 0 )
            &* setVk @"back"
                (  set @"failOp" VK_STENCIL_OP_KEEP
                &* set @"passOp" VK_STENCIL_OP_KEEP
                &* set @"depthFailOp" VK_STENCIL_OP_KEEP
                &* set @"compareOp" VK_COMPARE_OP_NEVER
                &* set @"compareMask" 0
                &* set @"writeMask" 0
                &* set @"reference" 0 )

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
            &* set @"pDynamicState" VK_NULL
            &* set @"layout" pipelineLayout
            &* set @"renderPass" renderPass
            &* set @"subpass" 0
            &* set @"basePipelineHandle" VK_NULL_HANDLE
            &* set @"basePipelineIndex" (-1)

    logInfo $ "Create Pipeline: " ++ show imageExtent
    graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
        withPtr graphicsPipelineCreateInfo $ \graphicsPipelineCreateInfoPtr -> do
            result <- vkCreateGraphicsPipelines device VK_NULL_HANDLE 1 graphicsPipelineCreateInfoPtr VK_NULL graphicsPipelinePtr
            validationVK result "vkCreatePipelines failed!"
        peek graphicsPipelinePtr

    return GraphicsPipelineData
        { _vertexShaderCreateInfo = vertexShaderCreateInfo
        , _fragmentShaderCreateInfo = fragmentShaderCreateInfo
        , _descriptorSetLayout = descriptorSetLayout
        , _pipelineLayout = pipelineLayout
        , _pipeline = graphicsPipeline }


destroyGraphicsPipeline :: VkDevice -> GraphicsPipelineData -> IO ()
destroyGraphicsPipeline device graphicsPipelineData@GraphicsPipelineData {..} = do
    logInfo $ "Destroy GraphicsPipeline"
    vkDestroyPipeline device _pipeline VK_NULL
    destroyPipelineLayout device _pipelineLayout
    destroyDescriptorSetLayout device _descriptorSetLayout
    destroyShaderStageCreateInfo device _vertexShaderCreateInfo
    destroyShaderStageCreateInfo device _fragmentShaderCreateInfo

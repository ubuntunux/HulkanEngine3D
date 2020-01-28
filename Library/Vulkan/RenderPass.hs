{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan.RenderPass
    ( RenderPassData (..)
    , GraphicsPipelineData (..)
    , createRenderPass
    , destroyRenderPass
    , createGraphicsPipeline
    , destroyGraphicsPipeline
    ) where

import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create

import Library.Utils
import Library.Logger
import Library.Vulkan.FrameBuffer
import Library.Vulkan.Shader


data GraphicsPipelineData = GraphicsPipelineData
    { _vertexShaderCreateInfo :: VkPipelineShaderStageCreateInfo
    , _fragmentShaderCreateInfo :: VkPipelineShaderStageCreateInfo
    , _pipelineLayout :: VkPipelineLayout
    , _pipeline :: VkPipeline
    } deriving (Eq, Show)

data RenderPassData = RenderPassData
    { _graphicsPipelineData :: GraphicsPipelineData
    , _renderPass :: VkRenderPass
    , _frameBufferData :: FrameBufferData
    , _clearValues :: [Float]
    } deriving (Eq, Show)


createRenderPass :: VkDevice -> VkFormat -> IO VkRenderPass
createRenderPass device imageFormat =
  let
    colorAttachment :: VkAttachmentDescription
    colorAttachment = createVk @VkAttachmentDescription
      $  set @"flags" VK_ZERO_FLAGS
      &* set @"format" imageFormat
      &* set @"samples" VK_SAMPLE_COUNT_1_BIT
      &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
      &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
      &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
      &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
      &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
      &* set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

    colorAttachmentRef :: VkAttachmentReference
    colorAttachmentRef = createVk @VkAttachmentReference
      $  set @"attachment" 0
      &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

    subpass :: VkSubpassDescription
    subpass = createVk @VkSubpassDescription
      $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
      &* set @"colorAttachmentCount" 1
      &* setVkRef @"pColorAttachments" colorAttachmentRef
      &* set @"pPreserveAttachments" VK_NULL
      &* set @"pInputAttachments" VK_NULL

    dependency :: VkSubpassDependency
    dependency = createVk @VkSubpassDependency
      $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
      &* set @"dstSubpass" 0
      &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* set @"srcAccessMask" VK_ZERO_FLAGS
      &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* set @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)

    renderPassCreateInfo :: VkRenderPassCreateInfo
    renderPassCreateInfo = createVk @VkRenderPassCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"attachmentCount" 1
      &* setVkRef @"pAttachments" colorAttachment
      &* set @"subpassCount" 1
      &* setVkRef @"pSubpasses" subpass
      &* set @"dependencyCount" 1
      &* setVkRef @"pDependencies" dependency
  in do
    renderPass <- alloca $ \renderPassPtr -> do
        result <- vkCreateRenderPass device (unsafePtr renderPassCreateInfo) VK_NULL renderPassPtr
        validationVK result "vkCreatePipelineLayout failed!"
        peek renderPassPtr
    touchVkData renderPassCreateInfo
    logInfo $ "Create RenderPass: " ++ show imageFormat
    return renderPass


destroyRenderPass :: VkDevice -> VkRenderPass -> IO ()
destroyRenderPass device renderPass = do
  logInfo "Destroy RenderPass"
  vkDestroyRenderPass device renderPass VK_NULL


createPipelineLayout :: VkDevice -> IO VkPipelineLayout
createPipelineLayout device = do
  let
    pipelineCreateInfo :: VkPipelineLayoutCreateInfo
    pipelineCreateInfo = createVk @VkPipelineLayoutCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"setLayoutCount" 0
      &* set @"pSetLayouts" VK_NULL
      &* set @"pushConstantRangeCount" 0
      &* set @"pPushConstantRanges" VK_NULL

  pipelineLayout <- alloca $ \pipelineLayoutPtr -> do
      result <- vkCreatePipelineLayout device (unsafePtr pipelineCreateInfo) VK_NULL pipelineLayoutPtr
      validationVK result "vkCreatePipelineLayout failed!"
      peek pipelineLayoutPtr

  logInfo $ "Create PipelineLayout : " ++ (show pipelineLayout)

  touchVkData pipelineCreateInfo
  return pipelineLayout


destroyPipelineLayout :: VkDevice -> VkPipelineLayout -> IO ()
destroyPipelineLayout device pipelineLayout = do
  logInfo "Destroy PipelineLayout"
  vkDestroyPipelineLayout device pipelineLayout VK_NULL


createGraphicsPipeline :: VkDevice
                       -> VkExtent2D
                       -> String
                       -> String
                       -> VkRenderPass
                       -> IO GraphicsPipelineData
createGraphicsPipeline device imageExtent vertexShaderFile fragmentShaderFile renderPass = do
  vertexShaderCreateInfo <- createShaderStageCreateInfo device vertexShaderFile VK_SHADER_STAGE_VERTEX_BIT
  fragmentShaderCreateInfo <- createShaderStageCreateInfo device fragmentShaderFile VK_SHADER_STAGE_FRAGMENT_BIT
  let shaderStageInfos = [vertexShaderCreateInfo, fragmentShaderCreateInfo]
  pipelineLayout <- createPipelineLayout device
  let
    vertexInputInfo :: VkPipelineVertexInputStateCreateInfo
    vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"vertexBindingDescriptionCount" 0
      &* set @"pVertexBindingDescriptions" VK_NULL
      &* set @"vertexAttributeDescriptionCount" 0
      &* set @"pVertexAttributeDescriptions" VK_NULL

    inputAssembly :: VkPipelineInputAssemblyStateCreateInfo
    inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
      &* set @"primitiveRestartEnable" VK_FALSE

    viewPort :: VkViewport
    viewPort = createVk @VkViewport
      $  set @"x" 0
      &* set @"y" 0
      &* set @"width" (fromIntegral $ getField @"width" imageExtent)
      &* set @"height" (fromIntegral $ getField @"height" imageExtent)
      &* set @"minDepth" 0
      &* set @"maxDepth" 1

    scissorRect :: VkRect2D
    scissorRect = createVk @VkRect2D
      $  set   @"extent" imageExtent
      &* setVk @"offset" ( set @"x" 0 &* set @"y" 0 )

    viewPortState :: VkPipelineViewportStateCreateInfo
    viewPortState = createVk @VkPipelineViewportStateCreateInfo
      $ set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"viewportCount" 1
      &* setVkRef @"pViewports" viewPort
      &* set @"scissorCount" 1
      &* setVkRef @"pScissors" scissorRect

    rasterizer :: VkPipelineRasterizationStateCreateInfo
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

    multisampling :: VkPipelineMultisampleStateCreateInfo
    multisampling = createVk @VkPipelineMultisampleStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"sampleShadingEnable" VK_FALSE
      &* set @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT
      &* set @"minSampleShading" 1.0
      &* set @"pSampleMask" VK_NULL
      &* set @"alphaToCoverageEnable" VK_FALSE
      &* set @"alphaToOneEnable" VK_FALSE

    colorBlendAttachment :: VkPipelineColorBlendAttachmentState
    colorBlendAttachment = createVk @VkPipelineColorBlendAttachmentState
      $  set @"colorWriteMask" (VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT)
      &* set @"blendEnable" VK_FALSE
      &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE
      &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO
      &* set @"colorBlendOp" VK_BLEND_OP_ADD
      &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
      &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
      &* set @"alphaBlendOp" VK_BLEND_OP_ADD

    colorBlending :: VkPipelineColorBlendStateCreateInfo
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

    getGraphicsPipelineCreateInfo :: Ptr VkPipelineShaderStageCreateInfo -> Word32 -> VkGraphicsPipelineCreateInfo
    getGraphicsPipelineCreateInfo shaderStageInfosPtr shaderStageInfoCount = createVk @VkGraphicsPipelineCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" VK_ZERO_FLAGS
      &* set @"stageCount" shaderStageInfoCount
      &* set @"pStages" shaderStageInfosPtr
      &* setVkRef @"pVertexInputState" vertexInputInfo
      &* setVkRef @"pInputAssemblyState" inputAssembly
      &* set @"pTessellationState" VK_NULL
      &* setVkRef @"pViewportState" viewPortState
      &* setVkRef @"pRasterizationState" rasterizer
      &* setVkRef @"pMultisampleState" multisampling
      &* set @"pDepthStencilState" VK_NULL
      &* setVkRef @"pColorBlendState" colorBlending
      &* set @"pDynamicState" VK_NULL
      &* set @"layout" pipelineLayout
      &* set @"renderPass" renderPass
      &* set @"subpass" 0
      &* set @"basePipelineHandle" VK_NULL_HANDLE
      &* set @"basePipelineIndex" (-1)

  logInfo $ "Create Pipeline: " ++ show imageExtent
  shaderStageInfosPtr <- newArray shaderStageInfos
  let graphicsPipelineCreateInfo = getGraphicsPipelineCreateInfo shaderStageInfosPtr (fromIntegral $ length shaderStageInfos)
  createGraphicsPipelinesFunc <- vkGetDeviceProc @VkCreateGraphicsPipelines device

  graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
      result <- createGraphicsPipelinesFunc device VK_NULL_HANDLE 1 (unsafePtr graphicsPipelineCreateInfo) VK_NULL graphicsPipelinePtr
      validationVK result "vkCreatePipelines failed!"
      peek graphicsPipelinePtr

  touchVkData graphicsPipelineCreateInfo
  free shaderStageInfosPtr

  return GraphicsPipelineData
    { _vertexShaderCreateInfo = vertexShaderCreateInfo
    , _fragmentShaderCreateInfo = fragmentShaderCreateInfo
    , _pipelineLayout = pipelineLayout
    , _pipeline = graphicsPipeline }


destroyGraphicsPipeline :: VkDevice -> GraphicsPipelineData -> IO ()
destroyGraphicsPipeline device graphicsPipelineData = do
  logInfo $ "Destroy GraphicsPipeline"
  let GraphicsPipelineData {..} = graphicsPipelineData
  vkDestroyPipeline device _pipeline VK_NULL
  destroyPipelineLayout device _pipelineLayout
  destroyShaderStageCreateInfo device _vertexShaderCreateInfo
  destroyShaderStageCreateInfo device _fragmentShaderCreateInfo

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Library.Shader
  ( createShaderStageCreateInfo
  , destroyShaderStageCreateInfo
  , createPipelineLayout
  , destroyPipelineLayout
  , createRenderPass
  , destroyRenderPass
  , createGraphicsPipeline
  , destroyGraphicsPipeline
  , compileGLSL
  )  where

import Control.Arrow (first, second)
import Control.Exception
import Control.Monad (unless, when)
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr (wordPtrToPtr)
import GHC.Ptr (Ptr (..), castPtr)
import Language.Haskell.TH
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Lib.Utils
import Library.Vulkan


compileGLSL :: FilePath -> IO (Int, Ptr Word32)
compileGLSL filePath = do
  validatorExe <- fromMaybe
        ( error $ unlines
          [ "Cannot find glslangValidator executable."
          , "Check if it is available in your $PATH."
          , "Read more about it at https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"]
        ) <$> findExecutable "glslangValidator"
  tmpDir <- getTemporaryDirectory
  curDir <- getCurrentDirectory
  createDirectoryIfMissing True tmpDir
  let spirvCodeFile = tmpDir </> "haskell-spirv2.tmp"
      shaderFile = curDir </> filePath
      shaderDir = takeDirectory shaderFile
      shaderFName = takeFileName shaderFile
  doesFileExist shaderFile >>= flip unless (error $ "compileGLSL: " ++ shaderFile ++ " does not exist.")
  doesFileExist spirvCodeFile >>= flip when (removeFile spirvCodeFile)

  (exitCode, stdo, stde) <- readCreateProcessWithExitCode
      ((shell $ validatorExe ++ " -V -o " ++ spirvCodeFile ++ " " ++ shaderFName) { cwd = Just shaderDir }) ""

  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure i -> do
      putStrLn stdo
      putStrLn stde
      error $ "glslangValidator exited with code " ++ show i ++ "."

  withBinaryFile spirvCodeFile ReadMode $ \h -> do
    fsize <- hFileSize h
    let contentSize = fromIntegral $ case rem fsize 4 of
          0 -> fsize
          k -> fsize + 4 - k
    contentsPtr <- mallocArray contentSize
    hasRead <- hGetBuf h contentsPtr contentSize
    unless (contentSize /= hasRead) $ do
      contents <- peekArray hasRead contentsPtr
      pokeArray contentsPtr (contents ++ (replicate (contentSize - hasRead) 0))
    return (contentSize, contentsPtr)

getShaderModuleCreateInfo :: Int -> Ptr Word32 -> IO VkShaderModuleCreateInfo
getShaderModuleCreateInfo codeSize codePtr = return $ createVk @VkShaderModuleCreateInfo
  $  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  &* set @"pNext" VK_NULL
  &* set @"codeSize" (fromIntegral codeSize)
  &* set @"pCode" codePtr
  &* set @"flags" 0

getShaderCreateInfo :: VkShaderStageFlagBits -> VkShaderModule -> VkPipelineShaderStageCreateInfo
getShaderCreateInfo stageBit shaderModule = createVk @VkPipelineShaderStageCreateInfo
  $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  &* set @"pNext" VK_NULL
  &* set @"stage" stageBit
  &* set @"module" shaderModule
  &* setStrRef @"pName" "main"

createShaderStageCreateInfo :: VkDevice -> String -> VkShaderStageFlagBits -> IO VkPipelineShaderStageCreateInfo
createShaderStageCreateInfo device shaderFilePath stageBit = do   
  (codeSize, codePtr) <- compileGLSL shaderFilePath
  shaderModuleCreateInfo <- getShaderModuleCreateInfo codeSize codePtr
  shaderModule <- alloca $ \shaderModulePtr -> do
    throwingVK "vkCreateShaderModule failed!"
      $ vkCreateShaderModule device (unsafePtr shaderModuleCreateInfo) VK_NULL shaderModulePtr
    peek shaderModulePtr
  touchVkData shaderModuleCreateInfo
  free codePtr
  return $ getShaderCreateInfo stageBit shaderModule

destroyShaderStageCreateInfo :: VkDevice -> VkPipelineShaderStageCreateInfo -> IO ()
destroyShaderStageCreateInfo device shaderStageCreateInfo = do
  vkDestroyShaderModule device (getField @"module" shaderStageCreateInfo) VK_NULL
  touchVkData shaderStageCreateInfo

createPipelineLayout :: VkDevice -> IO VkPipelineLayout
createPipelineLayout device = do
  pipelineLayout <- alloca $ \pipelineLayoutPtr -> do
    throwingVK "vkCreatePipelineLayout failed!"
      $ vkCreatePipelineLayout device (unsafePtr pipelineCreateInfo) VK_NULL pipelineLayoutPtr
    peek pipelineLayoutPtr
  touchVkData pipelineCreateInfo
  return pipelineLayout
  where
    pipelineCreateInfo = createVk @VkPipelineLayoutCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" 0
      &* set @"setLayoutCount" 0      
      &* set @"pSetLayouts" VK_NULL
      &* set @"pushConstantRangeCount" 0      
      &* set @"pPushConstantRanges" VK_NULL

destroyPipelineLayout :: VkDevice -> VkPipelineLayout -> IO ()
destroyPipelineLayout device pipelineLayout = vkDestroyPipelineLayout device pipelineLayout VK_NULL


createRenderPass :: VkDevice -> SwapChainData -> IO VkRenderPass
createRenderPass device swapChainData =
  let
    colorAttachment :: VkAttachmentDescription
    colorAttachment = createVk @VkAttachmentDescription
      $  set @"flags" 0
      &* set @"format" (swapChainImageFormat swapChainData)
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
      &* set @"srcAccessMask" 0
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
      throwingVK "vkCreatePipelineLayout failed!"
        $ vkCreateRenderPass device (unsafePtr renderPassCreateInfo) VK_NULL renderPassPtr
      peek renderPassPtr
    touchVkData renderPassCreateInfo
    return renderPass

destroyRenderPass :: VkDevice -> VkRenderPass -> IO ()
destroyRenderPass device renderPass = vkDestroyRenderPass device renderPass VK_NULL


createGraphicsPipeline :: VkDevice
                       -> SwapChainData
                       -> (Word32, Ptr VkPipelineShaderStageCreateInfo)
                       -> VkRenderPass
                       -> VkPipelineLayout
                       -> IO VkPipeline
createGraphicsPipeline device swapChainData (shaderStageCount, shaderStageInfosPtr) renderPass pipelineLayout =
  let    
    vertexInputInfo :: VkPipelineVertexInputStateCreateInfo
    vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" 0
      &* set @"vertexBindingDescriptionCount" 0
      &* set @"pVertexBindingDescriptions" VK_NULL
      &* set @"vertexAttributeDescriptionCount" 0
      &* set @"pVertexAttributeDescriptions" VK_NULL

    inputAssembly :: VkPipelineInputAssemblyStateCreateInfo
    inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" 0
      &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
      &* set @"primitiveRestartEnable" VK_FALSE

    viewPort :: VkViewport
    viewPort = createVk @VkViewport
      $  set @"x" 0
      &* set @"y" 0
      &* set @"width" (fromIntegral $ getField @"width" (swapChainExtent swapChainData))
      &* set @"height" (fromIntegral $ getField @"height" (swapChainExtent swapChainData))
      &* set @"minDepth" 0
      &* set @"maxDepth" 1

    scissorRect :: VkRect2D
    scissorRect = createVk @VkRect2D
      $  set   @"extent" (swapChainExtent swapChainData)
      &* setVk @"offset" ( set @"x" 0 &* set @"y" 0 )

    viewPortState :: VkPipelineViewportStateCreateInfo
    viewPortState = createVk @VkPipelineViewportStateCreateInfo
      $ set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" 0
      &* set @"viewportCount" 1
      &* setVkRef @"pViewports" viewPort
      &* set @"scissorCount" 1
      &* setVkRef @"pScissors" scissorRect

    rasterizer :: VkPipelineRasterizationStateCreateInfo
    rasterizer = createVk @VkPipelineRasterizationStateCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" 0
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
      &* set @"flags" 0
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
      &* set @"flags" 0
      &* set @"logicOpEnable" VK_FALSE
      &* set @"logicOp" VK_LOGIC_OP_COPY
      &* set @"attachmentCount" 1
      &* setVkRef @"pAttachments" colorBlendAttachment
      &* setAt @"blendConstants" @0 0.0
      &* setAt @"blendConstants" @1 0.0
      &* setAt @"blendConstants" @2 0.0
      &* setAt @"blendConstants" @3 0.0

    graphicsPipelineCreateInfo :: VkGraphicsPipelineCreateInfo
    graphicsPipelineCreateInfo = createVk @VkGraphicsPipelineCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" 0
      &* set @"stageCount" (fromIntegral shaderStageCount)
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
  in do
    createGraphicsPipelinesFunc <- vkGetDeviceProc @VkCreateGraphicsPipelines device
    graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
      throwingVK "vkCreateGraphicsPipelines failed!"
        $ createGraphicsPipelinesFunc device VK_NULL_HANDLE 1 (unsafePtr graphicsPipelineCreateInfo) VK_NULL graphicsPipelinePtr
      peek graphicsPipelinePtr
    touchVkData graphicsPipelineCreateInfo
    return graphicsPipeline

destroyGraphicsPipeline :: VkDevice -> VkPipeline -> IO ()
destroyGraphicsPipeline device graphicsPipeline = vkDestroyPipeline device graphicsPipeline VK_NULL
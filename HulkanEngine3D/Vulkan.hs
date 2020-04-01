{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module HulkanEngine3D.Vulkan where

import Data.Bits
import Foreign.Storable

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame

import HulkanEngine3D.Utilities.System


data RenderFeatures = RenderFeatures
    { _anisotropyEnable :: VkBool32
    , _msaaSamples :: VkSampleCountFlagBits
    } deriving (Eq, Show)


getColorBlendMode_None :: VkPipelineColorBlendAttachmentState
getColorBlendMode_None = createVk @VkPipelineColorBlendAttachmentState
    $  set @"colorWriteMask" ( VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT )
    &* set @"blendEnable" VK_FALSE
    &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE
    &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO
    &* set @"colorBlendOp" VK_BLEND_OP_ADD
    &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
    &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
    &* set @"alphaBlendOp" VK_BLEND_OP_ADD

getColorClearValue :: [Float] -> VkClearValue
getColorClearValue colors = createVk @VkClearValue
        $ setVk @"color"
            $ setVec @"float32" (vec4 (colors !! 0) (colors !! 1) (colors !! 2) (colors !! 3))

getDepthStencilClearValue :: Float -> Word32 -> VkClearValue
getDepthStencilClearValue depthClearValue stencilClearValue = createVk @VkClearValue
    $ setVk @"depthStencil"
        $  set @"depth" depthClearValue
        &* set @"stencil" stencilClearValue

runCommandsOnce :: VkDevice
                -> VkCommandPool
                -> VkQueue
                -> (VkCommandBuffer -> IO ())
                -> IO ()
runCommandsOnce device commandPool commandQueue action = do
    let allocInfo = createVk @VkCommandBufferAllocateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
            &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
            &* set @"commandPool" commandPool
            &* set @"commandBufferCount" 1
            &* set @"pNext" VK_NULL

    allocaPeek $ \commandBufferPtr -> do
        withPtr allocInfo $ \allocInfoPtr -> do
            vkAllocateCommandBuffers device allocInfoPtr commandBufferPtr
        commandBuffer <- peek commandBufferPtr

        let beginInfo = createVk @VkCommandBufferBeginInfo
                $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                &* set @"pNext" VK_NULL

        withPtr beginInfo $ \beginInfoPtr -> do
            vkBeginCommandBuffer commandBuffer beginInfoPtr

        -- run action
        action commandBuffer

        vkEndCommandBuffer commandBuffer

        let submitInfo = createVk @VkSubmitInfo
                $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
                &* set @"pNext" VK_NULL
                &* set @"waitSemaphoreCount" 0
                &* set @"pWaitSemaphores"   VK_NULL
                &* set @"pWaitDstStageMask" VK_NULL
                &* set @"commandBufferCount" 1
                &* set @"pCommandBuffers" commandBufferPtr
                &* set @"signalSemaphoreCount" 0
                &* set @"pSignalSemaphores" VK_NULL

        {- TODO: a real app would need a better logic for waiting.

                 In the example below, we create a new fence every time we want to
                 execute a single command. Then, we attach this fence to our command.
                 vkWaitForFences makes the host (CPU) wait until the command is executed.
                 The other way to do this thing is vkQueueWaitIdle.

                 I guess, a good approach could be to pass the fence to this function
                 from the call site. The call site would decide when it wants to wait
                 for this command to finish.

                 Even if we don't pass the fence from outside, maybe we should create
                 the fence oustise of the innermost `locally` scope. This way, the
                 fence would be shared between calls (on the other hand, a possible
                 concurrency would be hurt in this case).
               -}
        --   fence <- createFence dev False
        --   withVkPtr submitInfo $ \siPtr ->
        --       vkQueueSubmit cmdQueue 1 siPtr fence
        --   fencePtr <- newArrayPtr [fence]
        --   vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)

        withPtr submitInfo $ \submitInfoPtr -> do
            result <- vkQueueSubmit commandQueue 1 submitInfoPtr VK_NULL_HANDLE
            validationVK result "vkQueueSubmit error"

        vkQueueWaitIdle commandQueue >>= flip validationVK "vkQueueWaitIdle error"

        vkFreeCommandBuffers device commandPool 1 commandBufferPtr
    return ()
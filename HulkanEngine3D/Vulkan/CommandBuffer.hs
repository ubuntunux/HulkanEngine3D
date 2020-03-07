{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.CommandBuffer
  ( createCommandPool
  , destroyCommandPool
  , createCommandBuffers
  , destroyCommandBuffers
  ) where

import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Queue


createCommandPool :: VkDevice -> QueueFamilyDatas -> IO VkCommandPool
createCommandPool device QueueFamilyDatas {..} = do
    let graphicsQueueIndex = (_graphicsQueueIndex _queueFamilyIndices)
        commandPoolCreateInfo = createVk @VkCommandPoolCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"queueFamilyIndex" graphicsQueueIndex
    logInfo $ "Create Command Pool: graphicsFamilyIndex(" ++ show graphicsQueueIndex ++ ")"
    allocaPeek $ \commandPoolPtr -> do
        withPtr commandPoolCreateInfo $ \createInfoPtr -> do
            result <- vkCreateCommandPool device createInfoPtr VK_NULL commandPoolPtr
            validationVK result "vkCreateCommandPool failed!"

destroyCommandPool :: VkDevice -> VkCommandPool -> IO ()
destroyCommandPool device commandPool = do
  logInfo $ "Destroy Command Pool: " ++ show commandPool
  vkDestroyCommandPool device commandPool VK_NULL


createCommandBuffers :: VkDevice
                     -> VkCommandPool
                     -> Word32
                     -> Ptr VkCommandBuffer
                     -> IO ()
createCommandBuffers device commandPool commandBufferCount commandBuffersPtr = do
    let allocationInfo = createVk @VkCommandBufferAllocateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"commandPool" commandPool
            &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
            &* set @"commandBufferCount" (fromIntegral commandBufferCount)
    withPtr allocationInfo $ \allocationInfoPtr -> do
        result <- vkAllocateCommandBuffers device allocationInfoPtr commandBuffersPtr
        validationVK result "vkAllocateCommandBuffers failed!"
    logInfo $ "Create Command Buffer: "  ++ show commandBufferCount ++ " " ++ (show commandBuffersPtr)


destroyCommandBuffers :: VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()
destroyCommandBuffers device commandPool bufferCount commandBuffersPtr = do
  vkFreeCommandBuffers device commandPool bufferCount commandBuffersPtr

module Library.Vulkan where

import Graphics.Vulkan

import {-# SOURCE #-} Library.Vulkan.RenderPass

data RendererData

recordCommandBuffer :: RendererData
                    -> RenderPassData
                    -> VkBuffer -- vertex data
                    -> (Word32, VkBuffer) -- nr of indices and index data
                    -> [VkDescriptorSet]
                    -> IO ()

runCommandsOnce :: VkDevice -> VkCommandPool -> VkQueue -> (VkCommandBuffer -> IO ()) -> IO ()
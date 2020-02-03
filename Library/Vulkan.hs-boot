module Library.Vulkan where

import Graphics.Vulkan

import {-# SOURCE #-} Library.Vulkan.RenderPass

data RendererData

recordCommandBuffer :: [VkCommandBuffer] -> RenderPassData -> IO ()

runCommandsOnce :: VkDevice -> VkCommandPool -> VkQueue -> (VkCommandBuffer -> IO ()) -> IO ()
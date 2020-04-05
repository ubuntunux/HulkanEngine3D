module HulkanEngine3D.Vulkan.Vulkan where

import Graphics.Vulkan

getColorClearValue :: [Float] -> VkClearValue

getDepthStencilClearValue :: Float -> Word32 -> VkClearValue

runCommandsOnce :: VkDevice -> VkCommandPool -> VkQueue -> (VkCommandBuffer -> IO ()) -> IO ()
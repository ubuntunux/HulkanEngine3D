module HulkanEngine3D.Vulkan where

import Graphics.Vulkan

runCommandsOnce :: VkDevice -> VkCommandPool -> VkQueue -> (VkCommandBuffer -> IO ()) -> IO ()
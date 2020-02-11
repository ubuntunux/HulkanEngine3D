module Library.Constants
    ( vulkanLayers
    , requireDeviceExtensions
    , invalidQueueIndex
    , swapChainImageCount
    , maxFrameCount
    ) where

import Graphics.Vulkan
import Graphics.Vulkan.Ext.VK_KHR_swapchain

vulkanLayers :: [String]
vulkanLayers = ["VK_LAYER_LUNARG_standard_validation"]

requireDeviceExtensions :: [CString]
requireDeviceExtensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

invalidQueueIndex :: Word32
invalidQueueIndex = maxBound

swapChainImageCount :: Word32
swapChainImageCount = 3

maxFrameCount :: Int
maxFrameCount = 2

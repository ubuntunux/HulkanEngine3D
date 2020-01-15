module Library.Constants
    ( vulkanLayers
    , requireDeviceExtensions
    , invalidQueueIndex
    , imageCount
    , maxFrameCount
    , convertToSecond
    ) where

import Graphics.Vulkan
import Graphics.Vulkan.Ext.VK_KHR_swapchain

vulkanLayers :: [String]
vulkanLayers = ["VK_LAYER_LUNARG_standard_validation"]

requireDeviceExtensions :: [CString]
requireDeviceExtensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

invalidQueueIndex :: Word32
invalidQueueIndex = maxBound

imageCount :: Word32
imageCount = 3 -- tripple buffering

maxFrameCount :: Int
maxFrameCount = 2

convertToSecond :: Double
convertToSecond = 10^12

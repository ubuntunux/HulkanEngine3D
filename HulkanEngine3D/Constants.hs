module HulkanEngine3D.Constants where

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

enableImmediateMode :: Bool
enableImmediateMode = True

near :: Float
near = 0.1

far :: Float
far = 2000.0

fov :: Float
fov = 60.0

cameraMoveSpeed :: Float
cameraMoveSpeed = 1.0

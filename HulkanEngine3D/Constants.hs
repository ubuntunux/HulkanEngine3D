module HulkanEngine3D.Constants where

import Graphics.Vulkan
import Graphics.Vulkan.Ext.VK_KHR_swapchain

engineName :: String
engineName = "HulkanEngine3D"

vulkanLayers :: [String]
vulkanLayers = ["VK_LAYER_LUNARG_standard_validation"]

requireDeviceExtensions :: [CString]
requireDeviceExtensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

invalidQueueIndex :: Word32
invalidQueueIndex = maxBound

swapChainImageCount :: Int
swapChainImageCount = 3

swapChainImageIndices :: [Int]
swapChainImageIndices = [0..(swapChainImageCount-1)]

maxFrameCount :: Int
maxFrameCount = 2

enableImmediateMode :: Bool
enableImmediateMode = True

enableValidationLayer :: Bool
enableValidationLayer = True

isConcurrentMode :: Bool
isConcurrentMode = True

msaaSampleCount :: VkSampleCountFlagBits
msaaSampleCount = VK_SAMPLE_COUNT_4_BIT

meterPerUnit :: Float
meterPerUnit = 1.0::Float

near :: Float
near = 0.1

far :: Float
far = 2000.0

fov :: Float
fov = 60.0

cameraMoveSpeed :: Float
cameraMoveSpeed = 2.0

cameraPanSpeed :: Float
cameraPanSpeed = 0.01

cameraRotationSpeed :: Float
cameraRotationSpeed = 0.005

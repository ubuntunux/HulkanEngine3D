module HulkanEngine3D.Constants where

import Graphics.Vulkan
import Graphics.Vulkan.Ext.VK_KHR_swapchain

engineName :: String
engineName = "HulkanEngine3D"

vulkanLayers :: [String]
vulkanLayers = ["VK_LAYER_LUNARG_standard_validation"]

requireDeviceExtensions :: [CString]
requireDeviceExtensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

depthFomats :: [VkFormat]
depthFomats = [VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT, VK_FORMAT_D16_UNORM_S8_UINT, VK_FORMAT_D16_UNORM]

depthStencilFormats :: [VkFormat]
depthStencilFormats = [VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT]

invalidQueueIndex :: Word32
invalidQueueIndex = maxBound

swapChainImageCount :: Int
swapChainImageCount = 3

swapChainImageIndices :: [Int]
swapChainImageIndices = [0..(swapChainImageCount-1)]

swapChainImageFormat :: VkFormat
swapChainImageFormat = VK_FORMAT_B8G8R8A8_UNORM

maxFrameCount :: Int
maxFrameCount = 2

descriptorSetCountAtOnce :: Int
descriptorSetCountAtOnce = swapChainImageCount

maxDescriptorPoolAllocCount :: Int
maxDescriptorPoolAllocCount = 100

enableImmediateMode :: Bool
enableImmediateMode = True

enableValidationLayer :: Bool
enableValidationLayer = True

isConcurrentMode :: Bool
isConcurrentMode = True

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

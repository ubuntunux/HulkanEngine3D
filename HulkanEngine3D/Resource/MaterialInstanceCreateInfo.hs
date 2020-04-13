module HulkanEngine3D.Resource.MaterialInstanceCreateInfo where

import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.Descriptor

data MaterialInstanceCreateInfo = MaterialInstanceCreateInfo
    { _renderPassData :: RenderPassData
    , _pipelineData :: PipelineData
    , _descriptorBufferOrImageInfosList :: [[DescriptorBufferOrImageInfo]]
    } deriving Show
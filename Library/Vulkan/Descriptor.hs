{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}

module Library.Vulkan.Descriptor
  ( createDescriptorPool
  , destroyDecriptorPool
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , createDescriptorSets
  , prepareDescriptorSet
  ) where

import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import Library.Utils


createDescriptorPool :: VkDevice -> Int -> IO VkDescriptorPool
createDescriptorPool device count = do
    let bufferPoolSize = createVk @VkDescriptorPoolSize
            $  set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
            &* set @"descriptorCount" (fromIntegral count)
        imagePoolSize = createVk @VkDescriptorPoolSize
            $  set @"type" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            &* set @"descriptorCount" (fromIntegral count)
        poolCreateInfo = createVk @VkDescriptorPoolCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* setListCountAndRef @"poolSizeCount" @"pPoolSizes"
                [bufferPoolSize, imagePoolSize]
            &* set @"maxSets" (fromIntegral count)

    descriptorPool <- allocaPeek $ \descriptorPoolPtr ->
        withPtr poolCreateInfo $ \poolCreateInfoPtr ->
            vkCreateDescriptorPool device poolCreateInfoPtr VK_NULL descriptorPoolPtr
    return descriptorPool


destroyDecriptorPool :: VkDevice -> VkDescriptorPool -> IO ()
destroyDecriptorPool device descriptorPool =
    vkDestroyDescriptorPool device descriptorPool VK_NULL


createDescriptorSetLayout :: VkDevice -> IO VkDescriptorSetLayout
createDescriptorSetLayout device = do
    let bufferlayoutBinding = createVk @VkDescriptorSetLayoutBinding
            $  set @"binding" 0
            &* set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
            &* set @"descriptorCount" 1
            &* set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT
            &* set @"pImmutableSamplers" VK_NULL
        imageLayoutBinding = createVk @VkDescriptorSetLayoutBinding
            $  set @"binding" 1
            &* set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            &* set @"descriptorCount" 1
            &* set @"stageFlags" VK_SHADER_STAGE_FRAGMENT_BIT
            &* set @"pImmutableSamplers" VK_NULL
        layoutCreateInfo = createVk @VkDescriptorSetLayoutCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* setListCountAndRef @"bindingCount" @"pBindings"
                [bufferlayoutBinding, imageLayoutBinding]

    descriptorSetLayoutPtr <- allocaPtr
    withPtr layoutCreateInfo $ \layoutCreateInfoPtr ->
        vkCreateDescriptorSetLayout device layoutCreateInfoPtr VK_NULL descriptorSetLayoutPtr
    peek descriptorSetLayoutPtr


destroyDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayout -> IO ()
destroyDescriptorSetLayout device descriptorSetLayout =
    vkDestroyDescriptorSetLayout device descriptorSetLayout VK_NULL


createDescriptorSets :: VkDevice
                     -> VkDescriptorPool
                     -> Int
                     -> Ptr VkDescriptorSetLayout
                     -> IO [VkDescriptorSet]
createDescriptorSets device descriptorPool count layoutsPtr = do
    let allocateInfo = createVk @VkDescriptorSetAllocateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"descriptorPool" descriptorPool
            &* set @"descriptorSetCount" (fromIntegral count)
            &* set @"pSetLayouts" layoutsPtr
    descriptorSetPtr <- allocaArrayPtr count
    withPtr allocateInfo $ \allocateInfoPtr ->
        vkAllocateDescriptorSets device allocateInfoPtr descriptorSetPtr
    peekArray count descriptorSetPtr


prepareDescriptorSet :: VkDevice
                     -> VkDescriptorBufferInfo
                     -> VkDescriptorImageInfo
                     -> VkDescriptorSet
                     -> IO ()
prepareDescriptorSet device bufferInfo imageInfo descriptorSet = do
    let bufferDescriptorSet = createVk @VkWriteDescriptorSet
            $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
            &* set @"pNext" VK_NULL
            &* set @"dstSet" descriptorSet
            &* set @"dstBinding" 0
            &* set @"dstArrayElement" 0
            &* set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
            &* set @"descriptorCount" 1
            &* setVkRef @"pBufferInfo" bufferInfo
            &* set @"pImageInfo" VK_NULL
            &* set @"pTexelBufferView" VK_NULL
        imageDescriptorSet = createVk @VkWriteDescriptorSet
            $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
            &* set @"pNext" VK_NULL
            &* set @"dstSet" descriptorSet
            &* set @"dstBinding" 1
            &* set @"dstArrayElement" 0
            &* set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            &* set @"descriptorCount" 1
            &* set @"pBufferInfo" VK_NULL
            &* setVkRef @"pImageInfo" imageInfo
            &* set @"pTexelBufferView" VK_NULL
        descriptorWrites = [bufferDescriptorSet, imageDescriptorSet]
    withVkArrayLen descriptorWrites $ \count descriptorWritesPtr ->
        vkUpdateDescriptorSets device count descriptorWritesPtr 0 VK_NULL


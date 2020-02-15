{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}


module Library.Vulkan.Descriptor
  ( DescriptorSetData (..)
  , createDescriptorPool
  , destroyDescriptorPool
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , createDescriptorSetData
  , destroyDescriptorSetData
  , prepareDescriptorSet
  ) where

import Foreign.Ptr
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import Library.Utilities.System
import Library.Utilities.Logger


data DescriptorSetData = DescriptorSetData
    { _descriptorSetPtr :: Ptr VkDescriptorSet
    , _descriptorSets :: [VkDescriptorSet]
    , _descriptorSetCount :: Word32
    } deriving (Eq, Show)


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
            &* set @"flags" VK_ZERO_FLAGS -- manually free descriptorSets - VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
            &* setListCountAndRef @"poolSizeCount" @"pPoolSizes"
                [bufferPoolSize, imagePoolSize]
            &* set @"maxSets" (fromIntegral count)

    descriptorPool <- allocaPeek $ \descriptorPoolPtr ->
        withPtr poolCreateInfo $ \poolCreateInfoPtr ->
            vkCreateDescriptorPool device poolCreateInfoPtr VK_NULL descriptorPoolPtr
    logInfo $ "createDescriptorPool : " ++ show descriptorPool
    return descriptorPool


destroyDescriptorPool :: VkDevice -> VkDescriptorPool -> IO ()
destroyDescriptorPool device descriptorPool = do
    logInfo $ "destroyDescriptorPool : " ++ show descriptorPool
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

    descriptorSetLayout <- allocaPeek $ \descriptorSetLayoutPtr ->
        withPtr layoutCreateInfo $ \layoutCreateInfoPtr ->
            vkCreateDescriptorSetLayout device layoutCreateInfoPtr VK_NULL descriptorSetLayoutPtr
    logInfo $ "createDescriptorSetLayout : " ++ show descriptorSetLayout
    return descriptorSetLayout

destroyDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayout -> IO ()
destroyDescriptorSetLayout device descriptorSetLayout = do
    logInfo $ "destroyDescriptorSetLayout : " ++ show descriptorSetLayout
    vkDestroyDescriptorSetLayout device descriptorSetLayout VK_NULL

createDescriptorSetData :: VkDevice
                        -> VkDescriptorPool
                        -> Int
                        -> VkDescriptorSetLayout
                        -> IO DescriptorSetData
createDescriptorSetData device descriptorPool swapChainImageCount descriptorSetLayout = do
    let descriptorSetLayouts = replicate swapChainImageCount descriptorSetLayout
    allocaArray (length descriptorSetLayouts) $ \descriptorSetLayoutsPtr -> do
        pokeArray descriptorSetLayoutsPtr descriptorSetLayouts

        let allocateInfo = createVk @VkDescriptorSetAllocateInfo
                $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                &* set @"pNext" VK_NULL
                &* set @"descriptorPool" descriptorPool
                &* set @"descriptorSetCount" (fromIntegral swapChainImageCount)
                &* set @"pSetLayouts" descriptorSetLayoutsPtr
        descriptorSets <- allocaPeekArray swapChainImageCount $ \descriptorSetPtr ->
            withPtr allocateInfo $ \allocateInfoPtr ->
                vkAllocateDescriptorSets device allocateInfoPtr descriptorSetPtr
        let descriptorSetData = DescriptorSetData
                { _descriptorSets = descriptorSets
                , _descriptorSetPtr = VK_NULL -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for createDescriptorPool
                , _descriptorSetCount = fromIntegral swapChainImageCount }
        logInfo $ "createDescriptorSets : " ++ show descriptorSetData
        return descriptorSetData

destroyDescriptorSetData :: VkDevice
                         -> VkDescriptorPool
                         -> DescriptorSetData
                         -> IO ()
destroyDescriptorSetData device descriptorPool descriptorSetData@DescriptorSetData{..} = do
    -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for createDescriptorPool
    logInfo $ "destroyDescriptorSetData : " ++ show descriptorSetData
    vkFreeDescriptorSets device descriptorPool _descriptorSetCount _descriptorSetPtr
        >>= flip validationVK "destroyDescriptorSetData failed!"

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




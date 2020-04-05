{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}


module HulkanEngine3D.Vulkan.Descriptor
  ( DescriptorSetData (..)
  , createDescriptorPool
  , destroyDescriptorPool
  , createDescriptorSetLayoutBinding
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , createDescriptorSetData
  , destroyDescriptorSetData
  , prepareDescriptorSet
  , createDescriptorBufferInfo
  ) where

import Foreign.Ptr
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger


data DescriptorSetData = DescriptorSetData
    { _descriptorSetPtr :: Ptr VkDescriptorSet
    , _descriptorSets :: [VkDescriptorSet]
    } deriving (Eq, Show)


createDescriptorPool :: VkDevice -> Int -> IO VkDescriptorPool
createDescriptorPool device descriptorCount = do
    let bufferPoolSize = createVk @VkDescriptorPoolSize
            $  set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
            &* set @"descriptorCount" (fromIntegral descriptorCount)
        imagePoolSize = createVk @VkDescriptorPoolSize
            $  set @"type" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            &* set @"descriptorCount" (fromIntegral descriptorCount)
        poolSizeList = [bufferPoolSize, imagePoolSize]
        poolCreateInfo = createVk @VkDescriptorPoolCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS -- manually free descriptorSets - VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
            &* set @"poolSizeCount" (fromIntegral $ length poolSizeList)
            &* setListRef @"pPoolSizes" poolSizeList
            &* set @"maxSets" (fromIntegral descriptorCount)

    descriptorPool <- allocaPeek $ \descriptorPoolPtr ->
        withPtr poolCreateInfo $ \poolCreateInfoPtr ->
            vkCreateDescriptorPool device poolCreateInfoPtr VK_NULL descriptorPoolPtr
    logInfo $ "createDescriptorPool : " ++ show descriptorPool
    return descriptorPool


destroyDescriptorPool :: VkDevice -> VkDescriptorPool -> IO ()
destroyDescriptorPool device descriptorPool = do
    logInfo $ "destroyDescriptorPool : " ++ show descriptorPool
    vkDestroyDescriptorPool device descriptorPool VK_NULL


createDescriptorSetLayoutBinding :: Int -> VkDescriptorType -> VkShaderStageFlagBits -> VkDescriptorSetLayoutBinding
createDescriptorSetLayoutBinding binding descriptorType shaderStageFlags =
  createVk @VkDescriptorSetLayoutBinding
              $  set @"binding" (fromINtegral binding)
              &* set @"descriptorType" descriptorType
              &* set @"descriptorCount" 1
              &* set @"stageFlags" shaderStageFlags
              &* set @"pImmutableSamplers" VK_NULL

createDescriptorSetLayout :: VkDevice -> [VkDescriptorSetLayoutBinding] -> IO VkDescriptorSetLayout
createDescriptorSetLayout device layoutBindingList = do
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
        layoutBindingList = [bufferlayoutBinding, imageLayoutBinding]
        layoutCreateInfo = createVk @VkDescriptorSetLayoutCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"bindingCount" (fromIntegral $ length layoutBindingList)
            &* setListRef @"pBindings" layoutBindingList

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
                        -> VkDescriptorSetLayout
                        -> IO DescriptorSetData
createDescriptorSetData device descriptorPool descriptorSetLayout = do
    let descriptorSetCount = Constants.swapChainImageCount
        descriptorSetLayouts = replicate descriptorSetCount descriptorSetLayout
    allocaArray (length descriptorSetLayouts) $ \descriptorSetLayoutsPtr -> do
        pokeArray descriptorSetLayoutsPtr descriptorSetLayouts

        let allocateInfo = createVk @VkDescriptorSetAllocateInfo
                $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                &* set @"pNext" VK_NULL
                &* set @"descriptorPool" descriptorPool
                &* set @"descriptorSetCount" (fromIntegral descriptorSetCount)
                &* set @"pSetLayouts" descriptorSetLayoutsPtr
        descriptorSets <- allocaPeekArray descriptorSetCount $ \descriptorSetPtr ->
            withPtr allocateInfo $ \allocateInfoPtr ->
                vkAllocateDescriptorSets device allocateInfoPtr descriptorSetPtr
        let descriptorSetData = DescriptorSetData
                { _descriptorSets = descriptorSets
                , _descriptorSetPtr = VK_NULL -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for createDescriptorPool
                }
        logInfo $ "createDescriptorSets : " ++ show descriptorSetData
        return descriptorSetData


destroyDescriptorSetData :: VkDevice
                         -> VkDescriptorPool
                         -> DescriptorSetData
                         -> IO ()
destroyDescriptorSetData device descriptorPool descriptorSetData@DescriptorSetData{..} = do
    -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for createDescriptorPool
    logInfo $ "destroyDescriptorSetData : " ++ show descriptorSetData
    vkFreeDescriptorSets device descriptorPool (fromIntegral . length $ _descriptorSets) _descriptorSetPtr
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


createDescriptorBufferInfo :: VkBuffer -> VkDeviceSize -> VkDescriptorBufferInfo
createDescriptorBufferInfo uniformBuffer size =
    createVk @VkDescriptorBufferInfo
        $  set @"buffer" uniformBuffer
        &* set @"offset" 0
        &* set @"range" size
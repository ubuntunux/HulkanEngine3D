{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}


module HulkanEngine3D.Vulkan.Descriptor
  ( DescriptorSetDataCreateInfo
  , DescriptorSetData (..)
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

import Control.Monad
import Foreign.Ptr
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger


type DescriptorSetDataCreateInfo = (VkDescriptorType, VkShaderStageFlagBits)

data DescriptorSetData = DescriptorSetData
    { _descriptorSetPtr :: Ptr VkDescriptorSet
    , _descriptorSets :: [VkDescriptorSet]
    , _descriptorSetLayoutBindingList :: [VkDescriptorSetLayoutBinding]
    , _descriptorPoolSizeList :: [VkDescriptorPoolSize]
    , _descriptorPool :: VkDescriptorPool
    , _descriptorSetLayout :: VkDescriptorSetLayout
    , _descriptorCount :: Int
    } deriving (Eq, Show)


createDescriptorPool :: VkDevice -> [VkDescriptorPoolSize] -> Int -> IO VkDescriptorPool
createDescriptorPool device poolSizeList descriptorCount = do
    let poolCreateInfo = createVk @VkDescriptorPoolCreateInfo
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

createDescriptorPoolSize :: VkDescriptorType -> Int -> VkDescriptorPoolSize
createDescriptorPoolSize descriptorType descriptorCount =
    createVk @VkDescriptorPoolSize
        $  set @"type" descriptorType
        &* set @"descriptorCount" (fromIntegral descriptorCount)

createDescriptorSetLayoutBinding :: Int -> VkDescriptorType -> VkShaderStageFlagBits -> VkDescriptorSetLayoutBinding
createDescriptorSetLayoutBinding binding descriptorType shaderStageFlags =
    createVk @VkDescriptorSetLayoutBinding
        $  set @"binding" (fromIntegral binding)
        &* set @"descriptorType" descriptorType
        &* set @"descriptorCount" 1
        &* set @"stageFlags" (bitToMask shaderStageFlags)
        &* set @"pImmutableSamplers" VK_NULL

createDescriptorSetLayout :: VkDevice -> [VkDescriptorSetLayoutBinding] -> IO VkDescriptorSetLayout
createDescriptorSetLayout device layoutBindingList = do
    let layoutCreateInfo = createVk @VkDescriptorSetLayoutCreateInfo
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
                        -> [DescriptorSetDataCreateInfo]
                        -> Int
                        -> IO DescriptorSetData
createDescriptorSetData device descriptorSetDataCreateInfoList descriptorCount = do
    descriptorLayoutBindingWithPoolSizeList <- forM (zip descriptorSetDataCreateInfoList [0..]) $ \((descriptorType, shaderStageFlags), binding) -> do
        let descriptorLayoutBinding = createDescriptorSetLayoutBinding binding descriptorType shaderStageFlags
            descriptorPoolSize = createDescriptorPoolSize descriptorType descriptorCount
        return (descriptorLayoutBinding, descriptorPoolSize)
    let (descriptorSetLayoutBindingList, descriptorPoolSizeList) = unzip descriptorLayoutBindingWithPoolSizeList

    descriptorSetLayout <- createDescriptorSetLayout device descriptorSetLayoutBindingList
    descriptorPool <- createDescriptorPool device descriptorPoolSizeList descriptorCount

    let descriptorSetLayouts = replicate descriptorCount descriptorSetLayout
    allocaArray (length descriptorSetLayouts) $ \descriptorSetLayoutsPtr -> do
        pokeArray descriptorSetLayoutsPtr descriptorSetLayouts

        let allocateInfo = createVk @VkDescriptorSetAllocateInfo
                $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                &* set @"pNext" VK_NULL
                &* set @"descriptorPool" descriptorPool
                &* set @"descriptorSetCount" (fromIntegral descriptorCount)
                &* set @"pSetLayouts" descriptorSetLayoutsPtr
        descriptorSets <- allocaPeekArray descriptorCount $ \descriptorSetPtr ->
            withPtr allocateInfo $ \allocateInfoPtr ->
                vkAllocateDescriptorSets device allocateInfoPtr descriptorSetPtr
        let descriptorSetData = DescriptorSetData
                { _descriptorSets = descriptorSets
                , _descriptorSetPtr = VK_NULL -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for vkFreeDescriptorSets
                , _descriptorSetLayoutBindingList = descriptorSetLayoutBindingList
                , _descriptorPoolSizeList = descriptorPoolSizeList
                , _descriptorPool = descriptorPool
                , _descriptorSetLayout = descriptorSetLayout
                , _descriptorCount = descriptorCount
                }
        logInfo $ "createDescriptorSets : " ++ show descriptorSetData
        return descriptorSetData


destroyDescriptorSetData :: VkDevice
                         -> DescriptorSetData
                         -> IO ()
destroyDescriptorSetData device descriptorSetData@DescriptorSetData{..} = do
    logInfo $ "destroyDescriptorSetData : " ++ show descriptorSetData
    -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for vkFreeDescriptorSets
    when (_descriptorSetPtr /= VK_NULL) $
        vkFreeDescriptorSets device _descriptorPool (fromIntegral . length $ _descriptorSets) _descriptorSetPtr
            >>= flip validationVK "destroyDescriptorSetData failed!"
    destroyDescriptorSetLayout device _descriptorSetLayout
    destroyDescriptorPool device _descriptorPool

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
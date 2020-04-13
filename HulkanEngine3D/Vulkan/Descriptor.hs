{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}


module HulkanEngine3D.Vulkan.Descriptor
  ( DescriptorBufferOrImageInfo (..)
  , DescriptorDataCreateInfo
  , DescriptorData (..)
  , defaultDescriptorData
  , createDescriptorPool
  , destroyDescriptorPool
  , createDescriptorSetLayoutBinding
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , createDescriptorData
  , destroyDescriptorData
  , createDescriptorSet
  , destroyDescriptorSet
  , updateDescriptorSets
  ) where

import Control.Monad
import Foreign.Ptr
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger

data DescriptorBufferOrImageInfo = DescriptorBufferInfo VkDescriptorBufferInfo | DescriptorImageInfo VkDescriptorImageInfo deriving (Eq, Show)
type DescriptorDataCreateInfo = (VkDescriptorType, VkShaderStageFlagBits)

data DescriptorData = DescriptorData
    { _descriptorSetLayoutBindingList :: [VkDescriptorSetLayoutBinding]
    , _descriptorPoolSizeList :: [VkDescriptorPoolSize]
    , _descriptorPool :: VkDescriptorPool
    , _descriptorSetLayout :: VkDescriptorSetLayout
    , _descriptorCount :: Int
    } deriving (Eq, Show)

defaultDescriptorData :: DescriptorData
defaultDescriptorData = DescriptorData
    { _descriptorSetLayoutBindingList = []
    , _descriptorPoolSizeList = []
    , _descriptorPool = VK_NULL
    , _descriptorSetLayout = VK_NULL
    , _descriptorCount = 0
    }

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


createDescriptorData :: VkDevice
                        -> [DescriptorDataCreateInfo]
                        -> Int
                        -> IO DescriptorData
createDescriptorData device descriptorDataCreateInfoList descriptorCount = do
    descriptorLayoutBindingWithPoolSizeList <- forM (zip descriptorDataCreateInfoList [0..]) $ \((descriptorType, shaderStageFlags), binding) -> do
        let descriptorLayoutBinding = createDescriptorSetLayoutBinding binding descriptorType shaderStageFlags
            descriptorPoolSize = createDescriptorPoolSize descriptorType descriptorCount
        return (descriptorLayoutBinding, descriptorPoolSize)
    let (descriptorSetLayoutBindingList, descriptorPoolSizeList) = unzip descriptorLayoutBindingWithPoolSizeList

    descriptorSetLayout <- createDescriptorSetLayout device descriptorSetLayoutBindingList
    descriptorPool <- createDescriptorPool device descriptorPoolSizeList descriptorCount
    let descriptorData = DescriptorData
            { _descriptorSetLayoutBindingList = descriptorSetLayoutBindingList
            , _descriptorPoolSizeList = descriptorPoolSizeList
            , _descriptorPool = descriptorPool
            , _descriptorSetLayout = descriptorSetLayout
            , _descriptorCount = descriptorCount
            }
    logInfo $ "createDescriptorData : pool " ++ show (_descriptorPool descriptorData) ++ ", layout " ++ show (_descriptorSetLayout descriptorData)
    return descriptorData


destroyDescriptorData :: VkDevice
                      -> DescriptorData
                      -> IO ()
destroyDescriptorData device descriptorData@DescriptorData{..} = do
    logInfo $ "destroyDescriptorData : pool " ++ show _descriptorPool ++ ", layout " ++ show _descriptorSetLayout
    destroyDescriptorSetLayout device _descriptorSetLayout
    destroyDescriptorPool device _descriptorPool


createDescriptorSet :: VkDevice -> DescriptorData -> IO [VkDescriptorSet]
createDescriptorSet device descriptorData@DescriptorData {..} = do
    let descriptorSetLayouts = replicate _descriptorCount _descriptorSetLayout
    allocaArray (length descriptorSetLayouts) $ \descriptorSetLayoutsPtr -> do
        pokeArray descriptorSetLayoutsPtr descriptorSetLayouts
        let allocateInfo = createVk @VkDescriptorSetAllocateInfo
                $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                &* set @"pNext" VK_NULL
                &* set @"descriptorPool" _descriptorPool
                &* set @"descriptorSetCount" (fromIntegral . length $ descriptorSetLayouts)
                &* set @"pSetLayouts" descriptorSetLayoutsPtr
        descriptorSets <- allocaPeekArray _descriptorCount $ \descriptorSetPtr ->
            withPtr allocateInfo $ \allocateInfoPtr ->
                vkAllocateDescriptorSets device allocateInfoPtr descriptorSetPtr
        return descriptorSets

destroyDescriptorSet :: VkDevice -> VkDescriptorPool -> [VkDescriptorSet] -> Ptr VkDescriptorSet -> IO ()
destroyDescriptorSet device descriptorPool descriptorSets descriptorSetPtr = do
    -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for vkFreeDescriptorSets
    when (descriptorSetPtr /= VK_NULL) $
        vkFreeDescriptorSets device descriptorPool (fromIntegral . length $ descriptorSets) descriptorSetPtr
            >>= flip validationVK "destroyDescriptorSetData failed!"

updateDescriptorSets :: VkDevice
                     -> VkDescriptorSet
                     -> [VkDescriptorSetLayoutBinding]
                     -> [DescriptorBufferOrImageInfo]
                     -> IO ()
updateDescriptorSets device descriptorSet descriptorSetLayoutBindingList descriptorBufferOrImageInfos = do
    let descriptorWrites = zipWith3 writeDescriptorSet [0..] descriptorSetLayoutBindingList descriptorBufferOrImageInfos
    withVkArrayLen descriptorWrites $ \count descriptorWritesPtr ->
        vkUpdateDescriptorSets device count descriptorWritesPtr 0 VK_NULL
    where
        writeDescriptorSet :: Word32 -> VkDescriptorSetLayoutBinding -> DescriptorBufferOrImageInfo -> VkWriteDescriptorSet
        writeDescriptorSet index descriptorSetLayoutBinding descriptorBufferOrImageInfo =
            createVk @VkWriteDescriptorSet
                $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
                &* set @"pNext" VK_NULL
                &* set @"dstSet" descriptorSet
                &* set @"dstBinding" index
                &* set @"dstArrayElement" 0
                &* set @"descriptorType" (getField @"descriptorType" descriptorSetLayoutBinding)
                &* set @"descriptorCount" 1
                &* case descriptorBufferOrImageInfo of
                        DescriptorBufferInfo bufferInfo ->
                            setVkRef @"pBufferInfo" bufferInfo
                            &* set @"pImageInfo" VK_NULL
                        DescriptorImageInfo imageInfo ->
                            set @"pBufferInfo" VK_NULL
                            &* setVkRef @"pImageInfo" imageInfo
                &* set @"pTexelBufferView" VK_NULL
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.MaterialInstance where

import Control.Monad
import qualified Data.Text as Text
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor
import HulkanEngine3D.Utilities.Logger

data MaterialInstanceData = MaterialInstanceData
    { _materialInstanceName :: Text.Text
    , _renderPassData :: RenderPass.RenderPassData
    , _pipelineData :: RenderPass.PipelineData
    , _descriptorSetsPtr :: Ptr VkDescriptorSet
    , _writeDescriptorSetPtrs :: [Ptr VkWriteDescriptorSet]
    } deriving Show


createMaterialInstance :: VkDevice
                       -> Text.Text
                       -> RenderPass.RenderPassData
                       -> RenderPass.PipelineData
                       -> [[Descriptor.DescriptorResourceInfo]]
                       -> IO MaterialInstanceData
createMaterialInstance device materialInstanceName renderPassData pipelineData descriptorResourceInfosList = do
    logInfo $ "createMaterialInstance : " ++ Text.unpack materialInstanceName
    logInfo $ "    renderPass : " ++ Text.unpack (RenderPass._renderPassDataName renderPassData)
    logInfo $ "    pipeline : " ++ Text.unpack (RenderPass._pipelineDataName pipelineData)

    descriptorSets <- Descriptor.createDescriptorSet device (RenderPass._descriptorData pipelineData)
    descriptorSetsPtr <- mallocArray (length descriptorSets)
    pokeArray descriptorSetsPtr descriptorSets

    let descriptorData = RenderPass._descriptorData $ pipelineData
        descriptorBindingIndices = map Descriptor._descriptorBindingIndex' (Descriptor._descriptorDataCreateInfoList descriptorData)
        descriptorSetLayoutBindingList = Descriptor._descriptorSetLayoutBindingList descriptorData

    writeDescriptorSetPtrs <- forM (zip descriptorSets descriptorResourceInfosList) $ \(descriptorSet, descriptorResourceInfos) -> do
        let descriptorWrites = Descriptor.createWriteDescriptorSets descriptorSet descriptorBindingIndices descriptorSetLayoutBindingList descriptorResourceInfos
            count = length descriptorWrites
        writeDescriptorSetPtr <- mallocArray count
        pokeArray writeDescriptorSetPtr descriptorWrites
        vkUpdateDescriptorSets device (fromIntegral count) writeDescriptorSetPtr 0 VK_NULL
        return writeDescriptorSetPtr

    return MaterialInstanceData
        { _materialInstanceName = materialInstanceName
        , _renderPassData = renderPassData
        , _pipelineData = pipelineData
        , _descriptorSetsPtr = descriptorSetsPtr
        , _writeDescriptorSetPtrs = writeDescriptorSetPtrs
        }

destroyMaterialInstance :: VkDevice -> MaterialInstanceData -> IO ()
destroyMaterialInstance device materialInstanceData = do
    free (_descriptorSetsPtr materialInstanceData)
    mapM_ free (_writeDescriptorSetPtrs materialInstanceData)
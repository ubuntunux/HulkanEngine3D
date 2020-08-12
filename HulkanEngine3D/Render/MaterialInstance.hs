{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.MaterialInstance where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Render.Material as Material
import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor
import HulkanEngine3D.Utilities.Logger

data PipelineBindingData = PipelineBindingData
    { _descriptorSetsPtr :: Ptr VkDescriptorSet
    , _writeDescriptorSetPtrs :: [Ptr VkWriteDescriptorSet]
    } deriving Show

type PipelineBindingDataMap = Map.Map RenderPass.RenderPassPipelineDataName PipelineBindingData

data MaterialInstanceData = MaterialInstanceData
    { _materialInstanceDataName :: Text.Text
    , _materialData :: Material.MaterialData
    , _pipelineBindingDataMap :: PipelineBindingDataMap
    } deriving Show


createMaterialInstance :: VkDevice
                       -> Text.Text
                       -> Material.MaterialData
                       -> [(RenderPass.RenderPassData, RenderPass.PipelineData, [[Descriptor.DescriptorResourceInfo]])]
                       -> IO MaterialInstanceData
createMaterialInstance device materialInstanceDataName materialData pipelineBindingCreateInfoList = do
    logInfo $ "createMaterialInstance : " ++ Text.unpack materialInstanceDataName
    logInfo $ "    materialData : " ++ Text.unpack (Material._materialDataName materialData)
    pipelineBindingDataList <- forM pipelineBindingCreateInfoList $ \(renderPassData, pipelineData, descriptorResourceInfosList) -> do
        let renderPassPipelineDataName = (RenderPass._renderPassDataName renderPassData, RenderPass._pipelineDataName pipelineData)
        logInfo $ "        (RenderPass, Pipeline) : " ++ show renderPassPipelineDataName
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
        return (renderPassPipelineDataName, PipelineBindingData { _descriptorSetsPtr = descriptorSetsPtr, _writeDescriptorSetPtrs = writeDescriptorSetPtrs })
    return MaterialInstanceData
        { _materialInstanceDataName = materialInstanceDataName
        , _materialData = materialData
        , _pipelineBindingDataMap = Map.fromList pipelineBindingDataList
        }

destroyMaterialInstance :: VkDevice -> MaterialInstanceData -> IO ()
destroyMaterialInstance device materialInstanceData = do
    forM_ (_pipelineBindingDataMap materialInstanceData) $ \pipelineBindingData -> do
        free (_descriptorSetsPtr pipelineBindingData)
        mapM_ free (_writeDescriptorSetPtrs pipelineBindingData)

getPipelineBindingData :: PipelineBindingDataMap -> RenderPass.RenderPassPipelineDataName -> PipelineBindingData
getPipelineBindingData pipelineBindingDataMap renderPassPipelineDataName = Maybe.fromJust $ Map.lookup renderPassPipelineDataName pipelineBindingDataMap

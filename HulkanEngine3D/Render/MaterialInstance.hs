{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.MaterialInstance where

import Control.Monad

import Graphics.Vulkan

import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor

data MaterialInstanceData = MaterialInstanceData
    { _renderPassData :: RenderPass.RenderPassData
    , _pipelineData :: RenderPass.PipelineData
    , _descriptorSets :: [VkDescriptorSet]
    } deriving Show


createMaterialInstance :: VkDevice
                       -> RenderPass.RenderPassData
                       -> RenderPass.PipelineData
                       -> [[Descriptor.DescriptorResourceInfo]]
                       -> IO MaterialInstanceData
createMaterialInstance device renderPassData pipelineData descriptorResourceInfosList = do
    descriptorSets <- Descriptor.createDescriptorSet device (RenderPass._descriptorData pipelineData)

    forM_ (zip descriptorSets descriptorResourceInfosList) $ \(descriptorSet, descriptorResourceInfos) ->
        Descriptor.updateDescriptorSets device descriptorSet (Descriptor._descriptorSetLayoutBindingList . RenderPass._descriptorData $ pipelineData) descriptorResourceInfos

    return MaterialInstanceData
        { _renderPassData = renderPassData
        , _pipelineData = pipelineData
        , _descriptorSets = descriptorSets
        }

destroyMaterialInstance :: VkDevice -> MaterialInstanceData -> IO ()
destroyMaterialInstance device materialInstanceData = return ()
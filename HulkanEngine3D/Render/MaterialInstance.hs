{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.MaterialInstance
    ( MaterialInstanceData (..)
    , createMaterialInstance
    , destroyMaterialInstance
    ) where

import Graphics.Vulkan

import Control.Monad
import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor
import qualified HulkanEngine3D.Resource.MaterialInstanceCreateInfo as MaterialInstanceCreateInfo


data MaterialInstanceData = MaterialInstanceData
    { _renderPassData :: RenderPass.RenderPassData
    , _pipelineData :: RenderPass.PipelineData
    , _descriptorSets :: [VkDescriptorSet]
    } deriving Show


createMaterialInstance :: VkDevice -> MaterialInstanceCreateInfo.MaterialInstanceCreateInfo -> IO MaterialInstanceData
createMaterialInstance device materialInstanceCreateInfo = do
    let renderPassData = MaterialInstanceCreateInfo._renderPassData materialInstanceCreateInfo
        pipelineData = MaterialInstanceCreateInfo._pipelineData materialInstanceCreateInfo
        descriptorBufferOrImageInfosList = MaterialInstanceCreateInfo._descriptorBufferOrImageInfosList materialInstanceCreateInfo

    descriptorSets <- Descriptor.createDescriptorSet device (RenderPass._descriptorData pipelineData)

    forM_ (zip descriptorSets descriptorBufferOrImageInfosList) $ \(descriptorSet, descriptorBufferOrImageInfos) ->
        Descriptor.updateDescriptorSets device descriptorSet (Descriptor._descriptorSetLayoutBindingList . RenderPass._descriptorData $ pipelineData) descriptorBufferOrImageInfos

    return MaterialInstanceData
        { _renderPassData = renderPassData
        , _pipelineData = pipelineData
        , _descriptorSets = descriptorSets
        }

destroyMaterialInstance :: VkDevice -> MaterialInstanceData -> IO ()
destroyMaterialInstance device materialInstanceData = return ()
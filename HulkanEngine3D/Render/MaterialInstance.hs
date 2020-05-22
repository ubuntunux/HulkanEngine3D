{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.MaterialInstance where

import qualified Data.Text as Text
import Control.Monad

import Graphics.Vulkan

import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor
import HulkanEngine3D.Utilities.Logger

data MaterialInstanceData = MaterialInstanceData
    { _materialInstanceName :: Text.Text
    , _renderPassData :: RenderPass.RenderPassData
    , _pipelineData :: RenderPass.PipelineData
    , _descriptorSets :: [VkDescriptorSet]
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

    let descriptorData = RenderPass._descriptorData $ pipelineData
        descriptorBindingIndices = map Descriptor._descriptorBindingIndex' (Descriptor._descriptorDataCreateInfoList descriptorData)
        descriptorSetLayoutBindingList = Descriptor._descriptorSetLayoutBindingList descriptorData

    forM_ (zip descriptorSets descriptorResourceInfosList) $ \(descriptorSet, descriptorResourceInfos) ->
        Descriptor.updateDescriptorSets device descriptorSet descriptorBindingIndices descriptorSetLayoutBindingList descriptorResourceInfos

    return MaterialInstanceData
        { _materialInstanceName = materialInstanceName
        , _renderPassData = renderPassData
        , _pipelineData = pipelineData
        , _descriptorSets = descriptorSets
        }

destroyMaterialInstance :: VkDevice -> MaterialInstanceData -> IO ()
destroyMaterialInstance device materialInstanceData = return ()
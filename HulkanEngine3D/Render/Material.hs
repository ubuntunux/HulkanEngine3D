{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.Material where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import HulkanEngine3D.Utilities.Logger

data MaterialData = MaterialData
    { _materialDataName :: Text.Text
    , _renderPassPipelineDataMap :: Map.Map RenderPass.RenderPassPipelineDataName (RenderPass.RenderPassData, RenderPass.PipelineData)
    } deriving Show

createMaterial :: Text.Text
               -> [(RenderPass.RenderPassData, RenderPass.PipelineData)]
               -> IO MaterialData
createMaterial materialDataName renderPassPipelineDataList = do
    logInfo $ "createMaterial : " ++ Text.unpack materialDataName
    renderPassPipelineDataMap <- forM renderPassPipelineDataList $ \(renderPassData, pipelineData) -> do
        let renderPassPipelineDataName = (RenderPass._renderPassDataName renderPassData, RenderPass._pipelineDataName pipelineData)
        logInfo $ "    renderPass, pipeline : " ++ show renderPassPipelineDataName
        return (renderPassPipelineDataName, (renderPassData, pipelineData))
    return MaterialData
        { _materialDataName = materialDataName
        , _renderPassPipelineDataMap = Map.fromList renderPassPipelineDataMap
        }

destroyMaterial :: MaterialData -> IO ()
destroyMaterial materialData = return ()
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.PushConstant
    ( PushConstantData (..)
    , getPushConstantRange
    ) where

import GHC.Generics (Generic)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Numeric.DataFrame


data PushConstantData = PushConstantData
  { modelMatrix :: Mat44f
  } deriving (Eq, Ord, Show, Generic)

instance PrimBytes PushConstantData


getPushConstantRange :: PushConstantData -> VkShaderStageFlags -> VkPushConstantRange
getPushConstantRange pushConstantData shaderStage = createVk @VkPushConstantRange
    $ set @"stageFlags" shaderStage
    &* set @"size" (bSizeOf @PushConstantData pushConstantData)
    &* set @"offset" 0
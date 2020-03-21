{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.PushConstant
    ( PushConstantData
    , pushConstantRange
    ) where

import GHC.Generics (Generic)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame


data PushConstantData = PushConstantData
  { modelMatrix :: Mat44f
  } deriving (Eq, Ord, Show, Generic)

instance PrimBytes PushConstantData


pushConstantRange :: VkPushConstantRange
pushConstantRange = createVk @VkPushConstantRange
    $ set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT
    &* set @"size" (bSizeOf @PushConstantData undefined)
    &* set @"offset" 0
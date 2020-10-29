{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Render.PushConstant
    ( PushConstants_StaticRenderObject (..)
    , getPushConstantRange
    ) where

import GHC.Generics (Generic)
import Foreign.Storable

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import Numeric.DataFrame
import Numeric.PrimBytes


data PushConstants_StaticRenderObject = PushConstants_StaticRenderObject
  { modelMatrix :: Mat44f
  } deriving (Eq, Ord, Show, Generic)

instance PrimBytes PushConstants_StaticRenderObject

instance Storable PushConstants_StaticRenderObject where
    sizeOf _ = bSizeOf (undefined :: PushConstants_StaticRenderObject)
    alignment _ = bAlignOf (undefined :: PushConstants_StaticRenderObject)
    peek ptr = bPeek ptr
    poke ptr pushConstantData = bPoke ptr pushConstantData


class PushConstantsInterface a where
    getPushConstantRange :: a -> VkShaderStageFlags -> VkPushConstantRange

instance PushConstantsInterface PushConstants_StaticRenderObject where
    getPushConstantRange :: PushConstants_StaticRenderObject -> VkShaderStageFlags -> VkPushConstantRange
    getPushConstantRange pushConstantData shaderStage = createVk @VkPushConstantRange
        $ set @"stageFlags" shaderStage
        &* set @"size" (bSizeOf @PushConstants_StaticRenderObject pushConstantData)
        &* set @"offset" 0


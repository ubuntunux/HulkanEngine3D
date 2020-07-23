{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module HulkanEngine3D.Render.UniformBufferDatas where

import GHC.Generics (Generic)

import Graphics.Vulkan
import Data.Hashable
import qualified Data.HashTable.IO as HashTable
import Foreign.Storable

import Numeric.DataFrame
import Numeric.PrimBytes

import HulkanEngine3D.Vulkan.UniformBuffer
import HulkanEngine3D.Utilities.System

data UniformBufferType = UniformBuffer_SceneConstants
                       | UniformBuffer_ViewProjectionConstants
                       | UniformBuffer_LightConstants
                       | UniformBuffer_SSAOConstants
                       deriving (Enum, Eq, Ord, Show, Read, Generic)

instance Hashable UniformBufferType

type UniformBufferDataMap = HashTable.BasicHashTable UniformBufferType UniformBufferData

data SceneConstants = SceneConstants
    { _SCREEN_SIZE :: Vec2f
    , _BACKBUFFER_SIZE :: Vec2f
    , _TIME :: Scalar Float
    , _DELTA_TIME :: Scalar Float
    , _JITTER_FRAME :: Scalar Float
    , _SceneConstantsDummy0 :: Scalar Int
    } deriving (Show, Generic)

data ViewProjectionConstants = ViewProjectionConstants
  { _VIEW  :: Mat44f
  , _PROJECTION :: Mat44f
  , _VIEW_PROJECTION :: Mat44f
  , _INV_VIEW_PROJECTION :: Mat44f
  } deriving (Show, Generic)

data LightConstants = LightConstants
  { _SHADOW_VIEW_PROJECTION :: Mat44f
  , _LIGHT_POSITION :: Vec3f
  , _SHADOW_EXP :: Scalar Float
  , _LIGHT_DIRECTION :: Vec3f
  , _SHADOW_BIAS :: Scalar Float
  , _LIGHT_COLOR :: Vec3f
  , _SHADOW_SAMPLES :: Scalar Int
  } deriving (Show, Generic)

data SSAOConstants = SSAOConstants
  { _SSAO_SAMPLES :: DataFrame Float '[2, 4]
  } deriving (Show, Generic)

instance PrimBytes SceneConstants
instance PrimBytes ViewProjectionConstants
instance PrimBytes LightConstants
instance PrimBytes SSAOConstants

instance Storable SceneConstants where
    sizeOf _ = bSizeOf (undefined :: SceneConstants)
    alignment _ = bAlignOf (undefined :: SceneConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

instance Storable ViewProjectionConstants where
    sizeOf _ = bSizeOf (undefined :: ViewProjectionConstants)
    alignment _ = bAlignOf (undefined :: ViewProjectionConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

instance Storable LightConstants where
    sizeOf _ = bSizeOf (undefined :: LightConstants)
    alignment _ = bAlignOf (undefined :: LightConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

instance Storable SSAOConstants where
    sizeOf _ = bSizeOf (undefined :: SSAOConstants)
    alignment _ = bAlignOf (undefined :: SSAOConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

registUniformBufferDatas :: VkPhysicalDevice -> VkDevice -> UniformBufferDataMap -> IO ()
registUniformBufferDatas physicalDevice device uniformBufferDataMap = do
    registUniformBufferData uniformBufferDataMap UniformBuffer_SceneConstants (bSizeOf @SceneConstants undefined)
    registUniformBufferData uniformBufferDataMap UniformBuffer_ViewProjectionConstants (bSizeOf @ViewProjectionConstants undefined)
    registUniformBufferData uniformBufferDataMap UniformBuffer_LightConstants (bSizeOf @LightConstants undefined)
    registUniformBufferData uniformBufferDataMap UniformBuffer_SSAOConstants (bSizeOf @SSAOConstants undefined)
    where
        registUniformBufferData uniformBufferDataMap uniformBufferType sizeOfUniformBuffer = do
            uniformBufferData <- createUniformBufferData physicalDevice device (toText uniformBufferType) sizeOfUniformBuffer
            HashTable.insert uniformBufferDataMap uniformBufferType uniformBufferData

destroyUniformBufferDatas :: VkDevice -> UniformBufferDataMap -> IO ()
destroyUniformBufferDatas device uniformBufferDataMap = do
    clearHashTable uniformBufferDataMap (\(k, v) -> destroyUniformBufferData device v)
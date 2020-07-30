{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

module HulkanEngine3D.Render.RenderTargetDeclaration where

import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.HashTable.IO as HashTable

import qualified HulkanEngine3D.Vulkan.Texture as Texture

data RenderTargetType = RenderTarget_SceneColor
                      | RenderTarget_SceneDepth
                      | RenderTarget_BackBuffer
                      | RenderTarget_SceneAlbedo
                      | RenderTarget_SceneNormal
                      | RenderTarget_SceneMaterial
                      | RenderTarget_SceneVelocity
                      | RenderTarget_SSAO
                      | RenderTarget_Shadow
                      deriving (Enum, Eq, Ord, Show, Read, Generic)

instance Hashable RenderTargetType

type RenderTargetDataMap = HashTable.BasicHashTable RenderTargetType Texture.TextureData
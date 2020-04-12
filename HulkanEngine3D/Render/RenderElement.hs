{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Render.RenderElement
    ( RenderElementData (..)
    , defaultRenderElementData
    ) where

import Graphics.Vulkan

data RenderElementData = RenderElementData
    { _descriptorSets :: [VkDescriptorSet]
    } deriving (Eq, Show)

defaultRenderElementData :: RenderElementData
defaultRenderElementData = RenderElementData
    { _descriptorSets = []
    }
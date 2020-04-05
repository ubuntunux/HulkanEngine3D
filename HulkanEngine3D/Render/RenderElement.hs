{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Render.RenderElement
    ( RenderElementData (..)
    ) where

import Graphics.Vulkan



data RenderElementData = RenderElementData
    { _descriptorSets :: [VkDescriptorSet]
    } deriving (Eq, Show)
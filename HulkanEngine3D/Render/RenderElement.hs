{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Render.RenderElement
    ( RenderElementData (..)
    , defaultRenderElementData
    ) where

data RenderElementData = RenderElementData
    {
    } deriving (Eq, Show)

defaultRenderElementData :: RenderElementData
defaultRenderElementData = RenderElementData
    {
    }
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Render.MaterialInstance
    ( MaterialInstanceData (..)
    , defaultMaterialInstance
    , createMaterialInstance
    , destroyMaterialInstance
    ) where

import Graphics.Vulkan


data MaterialInstanceData = MaterialInstanceData
    {
    } deriving (Eq, Show)


defaultMaterialInstance :: MaterialInstanceData
defaultMaterialInstance = MaterialInstanceData
    {
    }

createMaterialInstance :: VkDevice -> IO MaterialInstanceData
createMaterialInstance device = do
    return MaterialInstanceData
        {
        }

destroyMaterialInstance :: VkDevice -> MaterialInstanceData -> IO ()
destroyMaterialInstance device materialInstanceData = return ()
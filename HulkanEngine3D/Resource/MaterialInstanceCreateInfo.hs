{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.MaterialInstanceCreateInfo where

import qualified Data.Text as Text

data MaterialParameterType = MaterialParameter_UniformBuffer Text.Text
                           | MaterialParameter_Texture Text.Text
                           | MaterialParameter_RenderTarget Text.Text
                           deriving (Eq, Show)


data MaterialInstanceCreateInfo = MaterialInstanceCreateInfo
    { _materialInstanceName' :: Text.Text
    , _materialInstanceRenderPassName' :: Text.Text
    , _materialInstancePipelineName' :: Text.Text
    , _materialParameterTypes' :: [MaterialParameterType]
    } deriving (Eq, Show)


getMaterialInstanceCreateInfo :: Text.Text -> MaterialInstanceCreateInfo
getMaterialInstanceCreateInfo materialInstanceName
    | "default" == materialInstanceName = MaterialInstanceCreateInfo
        { _materialInstanceName' = "default"
        , _materialInstanceRenderPassName' = "default"
        , _materialInstancePipelineName' = ""
        , _materialParameterTypes' =
            [ MaterialParameter_UniformBuffer "SceneConstantsData"
            , MaterialParameter_Texture "common/default"
            ]
        }
    | otherwise = undefined
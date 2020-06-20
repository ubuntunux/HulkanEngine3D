{-# LANGUAGE InstanceSigs               #-}

module HulkanEngine3D.Resource.ResourceData where

import qualified Data.Text as Text
--import qualified Data.Time as Time

import HulkanEngine3D.Render.Mesh

data MetaData = MetaData
    { _isEngineResource :: Bool
    , _metaFilePath :: Bool
    , _resourceDataType :: ResourceData
    , _resourceVersion :: Int
    , _resourceFilePath :: FilePath
    , _resourceModifyTime :: Text.Text
    , _sourceFilePath :: FilePath
    , _sourceModifyTime :: Text.Text
    , _sourceChanged :: Bool
    } deriving (Eq, Show)


data ResourceData = ResourceData { _data :: MeshData } deriving (Eq, Show)

{-# LANGUAGE InstanceSigs               #-}

module HulkanEngine3D.Resource.ResourceData where

import qualified Data.Text as Text
--import qualified Data.Time as Time

data MetaData = MetaData
    { _isEngineResource :: Bool
    , _metaFilePath :: Bool
    , _resourceVersion :: Int
    , _resourceFilePath :: FilePath
    , _resourceModifyTime :: Text.Text
    , _sourceFilePath :: FilePath
    , _sourceModifyTime :: Text.Text
    , _sourceChanged :: Bool
    } deriving (Eq, Show)
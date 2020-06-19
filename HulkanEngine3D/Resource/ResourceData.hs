{-# LANGUAGE InstanceSigs               #-}

module HulkanEngine3D.Resource.ResourceData where

import qualified Data.Text as Text
--import qualified Data.Time as Time

    { _metaDataMap :: MetaDataMap
    , _meshDataMap :: MeshDataMap
    , _modelDataMap :: ModelDataMap
    , _textureDataMap :: TextureDataMap
    , _frameBufferDataMap :: FrameBufferDataMap
    , _renderPassDataMap :: RenderPassDataMap
    , _materialInstanceDataMap :: MaterialInstanceDataMap
    , _descriptorDataMap :: DescriptorDataMap

data ResourceDataType = ResourceDataType_Mesh
                      | ResourceDataType_Model
                      | ResourceDataType_Texture
                      | ResourceDataType_FrameBuffer
                      | ResourceDataType_RenderPass
                      | ResourceDataType_MaterialInstance
                      | ResourceDataType_Descriptor
                      deriving (Eq, Show)

data MetaData = MetaData
    { _isEngineResource :: Bool
    , _metaFilePath :: Bool
    , _resourceDataType :: ResourceDataType
    , _resourceVersion :: Int
    , _resourceFilePath :: FilePath
    , _resourceModifyTime :: Text.Text
    , _sourceFilePath :: FilePath
    , _sourceModifyTime :: Text.Text
    , _sourceChanged :: Bool
    } deriving (Eq, Show)
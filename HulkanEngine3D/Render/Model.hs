{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}

module HulkanEngine3D.Render.Model
    ( ModelData (..)
    , ModelInterface (..)
    ) where

import Data.IORef
import Data.Text
import qualified Data.Vector.Mutable as MVector

import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.Mesh
import HulkanEngine3D.Utilities.System ()

data ModelData = ModelData
    { _name :: IORef Text
    , _meshData :: MeshData
    , _materialInstances :: MaterialInstanceDataList
    } deriving Show


class ModelInterface a where
    newModelData :: Text -> MeshData -> IO a
    updateModelData :: a -> IO ()

instance ModelInterface ModelData where
    newModelData name meshData = do
        nameRef <- newIORef name
        let geometryBufferDataCount = MVector.length (_geometryBufferDatas meshData)
        materialInstances <- MVector.new geometryBufferDataCount
        return ModelData
            { _name = nameRef
            , _meshData = meshData
            , _materialInstances = materialInstances
            }

    updateModelData modelData = return ()


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
    { _modelDataName :: IORef Text
    , _meshData :: MeshData
    , _materialInstances :: IORef MaterialInstanceDataList
    } deriving Show


class ModelInterface a where
    newModelData :: Text -> MeshData -> IO a
    updateModelData :: a -> IO ()

instance ModelInterface ModelData where
    newModelData name meshData = do
        modelDataName <- newIORef name
        geometryBufferDataCount <- getGeometryCount meshData
        materialInstances <- MVector.new geometryBufferDataCount
        materialInstancesRef <- newIORef materialInstances
        return ModelData
            { _modelDataName = modelDataName
            , _meshData = meshData
            , _materialInstances = materialInstancesRef
            }

    updateModelData modelData = return ()


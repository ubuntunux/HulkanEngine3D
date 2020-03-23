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

import HulkanEngine3D.Render.Mesh
import HulkanEngine3D.Utilities.System()


data ModelData = ModelData
    { _name :: IORef Text
    , _meshData :: MeshData
    , _materialInstances :: [Bool]
    } deriving (Show)


class ModelInterface a where
    newModelData :: Text -> MeshData -> IO a
    updateModelData :: a -> IO ()

instance ModelInterface ModelData where
    newModelData name meshData = do
        nameRef <- newIORef name
        return ModelData
            { _name = nameRef
            , _meshData = meshData
            , _materialInstances = []
            }

    updateModelData modelData = return ()


{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}

module HulkanEngine3D.Render.Mesh
    ( MeshData (..)
    , MeshInterface (..)
    ) where

import Data.IORef
import Data.Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import HulkanEngine3D.Vulkan.GeometryBuffer
import HulkanEngine3D.Utilities.System


data MeshData = MeshData
    { _name :: IORef Text
    , _boundBox :: Bool
    , _skeletonDatas :: [Bool]
    , _animationDatas :: [Bool]
    --, _geometryBufferDatas :: MVector.MVector () GeometryBufferData
    } deriving (Show)

class MeshInterface a where
    newMeshData :: Text -> IO a
    updateMeshData :: a -> IO ()

instance MeshInterface MeshData where
    newMeshData meshName = do
        nameRef <- newIORef meshName
        return MeshData
            { _name = nameRef
            , _boundBox = False
            , _skeletonDatas = []
            , _animationDatas = []
            }

    updateMeshData meshData = return ()


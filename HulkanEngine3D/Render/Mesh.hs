{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}

module HulkanEngine3D.Render.Mesh
    ( MeshData (..)
    , MeshInterface (..)
    , GeometryBufferDataList
    ) where

import Control.Monad
import Data.IORef
import qualified Data.Text as Text
import Control.Monad.Primitive
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import HulkanEngine3D.Vulkan.GeometryBuffer
import HulkanEngine3D.Utilities.System

type GeometryBufferDataList = MVector.IOVector GeometryBufferData

instance Show GeometryBufferDataList where
    show a = show . MVector.length $ a

data MeshData = MeshData
    { _name :: IORef Text.Text
    , _boundBox :: Bool
    , _skeletonDatas :: [Bool]
    , _animationDatas :: [Bool]
    , _geometryBufferDatas :: GeometryBufferDataList
    } deriving (Show)

class MeshInterface a where
    newMeshData :: Text.Text -> [GeometryBufferData] -> IO a
    updateMeshData :: a -> IO ()

instance MeshInterface MeshData where
    newMeshData :: Text.Text -> [GeometryBufferData] -> IO MeshData
    newMeshData meshName geometryBufferDatas = do
        nameRef <- newIORef meshName
        geometryBufferDataList <- MVector.new (length geometryBufferDatas)
        forM_ (zip [0..] geometryBufferDatas) $ \(index, geometryBufferData) ->
            MVector.write geometryBufferDataList index geometryBufferData
        return MeshData
            { _name = nameRef
            , _boundBox = False
            , _skeletonDatas = []
            , _animationDatas = []
            , _geometryBufferDatas = geometryBufferDataList
            }

    updateMeshData :: MeshData -> IO ()
    updateMeshData meshData = return ()
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
    , GeometryDataList
    ) where

import Data.IORef
import qualified Data.Text as Text

import HulkanEngine3D.Vulkan.GeometryBuffer
import HulkanEngine3D.Utilities.System ()

data MeshData = MeshData
    { _name :: IORef Text.Text
    , _boundBox :: Bool
    , _skeletonDatas :: [Bool]
    , _animationDatas :: [Bool]
    , _geometryBufferDatas :: IORef GeometryDataList
    } deriving (Eq, Show)

class MeshInterface a where
    newMeshData :: Text.Text -> [GeometryData] -> IO a
    getGeometryDataCount :: a -> IO Int
    getGeometryDataList :: a -> IO GeometryDataList
    getGeometryData :: a -> Int -> IO GeometryData
    updateMeshData :: a -> IO ()

instance MeshInterface MeshData where
    newMeshData :: Text.Text -> [GeometryData] -> IO MeshData
    newMeshData meshName geometryBufferDatas = do
        nameRef <- newIORef meshName
        geometryBufferDatasRef <- newIORef geometryBufferDatas
        return MeshData
            { _name = nameRef
            , _boundBox = False
            , _skeletonDatas = []
            , _animationDatas = []
            , _geometryBufferDatas = geometryBufferDatasRef
            }

    getGeometryDataCount :: MeshData -> IO Int
    getGeometryDataCount meshData = do
        geometryBufferDatasList <- readIORef (_geometryBufferDatas meshData)
        return $ length geometryBufferDatasList

    getGeometryDataList :: MeshData -> IO GeometryDataList
    getGeometryDataList meshData = readIORef (_geometryBufferDatas meshData)

    getGeometryData :: MeshData -> Int -> IO GeometryData
    getGeometryData meshData n = do
        geometryBufferDatasList <- readIORef (_geometryBufferDatas meshData)
        return $ geometryBufferDatasList !! n

    updateMeshData :: MeshData -> IO ()
    updateMeshData meshData = return ()
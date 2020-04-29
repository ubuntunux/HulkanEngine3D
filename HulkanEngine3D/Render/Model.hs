{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE InstanceSigs        #-}

module HulkanEngine3D.Render.Model where

import Data.IORef
import qualified Data.Text as T

import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.Mesh
import HulkanEngine3D.Utilities.System ()


data ModelCreateInfo = ModelCreateInfo
    { _modelDataName' :: T.Text
    , _meshData' :: MeshData
    , _materialInstanceDatas' :: [MaterialInstanceData]
    } deriving Show

data ModelData = ModelData
    { _modelDataName :: IORef T.Text
    , _meshData :: MeshData
    , _materialInstanceDatas :: IORef [MaterialInstanceData]
    } deriving Show


class ModelInterface a where
    newModelData :: T.Text -> MeshData -> [MaterialInstanceData] -> IO a
    destroyModelData :: a -> IO ()
    getMeshData :: a -> MeshData
    getMaterialInstanceDataCount :: a -> IO Int
    getMaterialInstanceDataList :: a -> IO [MaterialInstanceData]
    getMaterialInstanceData :: a -> Int -> IO MaterialInstanceData
    updateModelData :: a -> IO ()

instance ModelInterface ModelData where
    newModelData :: T.Text -> MeshData -> [MaterialInstanceData] -> IO ModelData
    newModelData name meshData materialInstanceDatas = do
        modelDataName <- newIORef name
        materialInstanceDatasRef <- newIORef materialInstanceDatas
        return ModelData
            { _modelDataName = modelDataName
            , _meshData = meshData
            , _materialInstanceDatas = materialInstanceDatasRef
            }

    destroyModelData :: ModelData -> IO ()
    destroyModelData modelData = do
        materialInstanceDatas <- readIORef (_materialInstanceDatas modelData)
        return ()

    getMeshData :: ModelData -> MeshData
    getMeshData modelData = _meshData modelData

    getMaterialInstanceDataCount :: ModelData -> IO Int
    getMaterialInstanceDataCount modelData = do
        materialInstanceDatas <- readIORef (_materialInstanceDatas modelData)
        return $ length materialInstanceDatas

    getMaterialInstanceDataList :: ModelData -> IO [MaterialInstanceData]
    getMaterialInstanceDataList modelData = readIORef (_materialInstanceDatas modelData)

    getMaterialInstanceData :: ModelData -> Int -> IO MaterialInstanceData
    getMaterialInstanceData modelData n = do
        materialInstanceDatas <- readIORef (_materialInstanceDatas modelData)
        return $ materialInstanceDatas !! n

    updateModelData :: ModelData -> IO ()
    updateModelData modelData = return ()


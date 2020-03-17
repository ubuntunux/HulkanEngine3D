{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.Resource
    ( ResourceData (..)
    , ResourceInterface (..)
    ) where

import qualified Data.HashTable.IO as HashTable
import Data.Text
import HulkanEngine3D.Render.Mesh
import HulkanEngine3D.Resource.ObjLoader
import HulkanEngine3D.Utilities.System

type MeshDataMap = HashTable.BasicHashTable Text MeshData

data ResourceData = ResourceData
    { _meshDataMap :: MeshDataMap
    } deriving (Show)

--, _textureData :: TextureData
--    , _geometryBufferData :: GeometryBufferData

class ResourceInterface a where
    createNewResourceData :: IO a
    initializeResourceData :: a -> IO ()
    updateResourceData :: a -> IO ()
    destroyResourceData :: a -> IO ()
    

instance ResourceInterface ResourceData where
    createNewResourceData :: IO ResourceData
    createNewResourceData = do
        meshDataMap <- HashTable.new
        return ResourceData
            { _meshDataMap = meshDataMap
            }

    initializeResourceData :: ResourceData -> IO ()
    initializeResourceData resourceData = return ()

    updateResourceData :: ResourceData -> IO ()
    updateResourceData resourceData = return ()

    destroyResourceData :: ResourceData -> IO ()
    destroyResourceData resourceData = return ()


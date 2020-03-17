{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Application.SceneManager
    ( SceneManagerData (..)
    , SceneManagerInterface (..)
    ) where

import qualified Data.HashTable.IO as HashTable
import Data.Text
import HulkanEngine3D.Render.Actor
import HulkanEngine3D.Render.Camera

type ObjectMap = HashTable.BasicHashTable Text Bool

data SceneManagerData = SceneManagerData
    { _camera :: CameraData
    , _objectMap :: ObjectMap
    } deriving (Show)

class SceneManagerInterface a where
    newSceneManagerData :: CameraData -> IO a
    updateSceneManagerData :: a -> a
    

instance SceneManagerInterface SceneManagerData where
    newSceneManagerData :: CameraData -> IO SceneManagerData
    newSceneManagerData cameraData = do
        objectMap <- HashTable.new
        return SceneManagerData
            { _camera = cameraData
            , _objectMap = objectMap
            }

    updateSceneManagerData :: SceneManagerData -> SceneManagerData
    updateSceneManagerData sceneManagerData = sceneManagerData


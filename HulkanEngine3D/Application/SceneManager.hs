module HulkanEngine3D.Application.SceneManager
    ( SceneManagerData (..)
    , SceneManagerInterface (..)
    ) where

import HulkanEngine3D.Render.Camera

data SceneManagerData = SceneManagerData
    { _camera :: CameraData
    } deriving (Show)

class SceneManagerInterface a where
    newSceneManagerData :: CameraData -> a
    updateSceneManagerData :: a -> a

instance SceneManagerInterface SceneManagerData where
    newSceneManagerData cameraData = SceneManagerData {
            _camera = cameraData
        }

    updateSceneManagerData sceneManagerData = sceneManagerData
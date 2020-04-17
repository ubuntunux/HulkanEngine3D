module HulkanEngine3D.Application.SceneManager where

import qualified Data.Text as T

import HulkanEngine3D.Render.Camera
import {-# SOURCE #-} qualified HulkanEngine3D.Render.Renderer as Renderer
import {-# SOURCE #-} HulkanEngine3D.Resource.Resource

data SceneManagerData

class SceneManagerInterface a where
    newSceneManagerData :: Renderer.RendererData -> ResourceData -> IO a
    loadSceneManagerData :: a -> CameraCreateData -> IO ()
    getMainCamera :: a -> IO CameraObjectData
    addCameraObject :: a -> T.Text -> CameraCreateData -> IO CameraObjectData
    updateSceneManagerData :: a -> IO ()

instance SceneManagerInterface SceneManagerData where
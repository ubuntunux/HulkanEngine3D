module HulkanEngine3D.Application.SceneManager where

import qualified Data.Text as T

import HulkanEngine3D.Render.RenderObject
import qualified HulkanEngine3D.Render.Camera as Camera
import qualified HulkanEngine3D.Render.Light as Light
import {-# SOURCE #-} qualified HulkanEngine3D.Render.Renderer as Renderer
import {-# SOURCE #-} HulkanEngine3D.Resource.Resource
import qualified HulkanEngine3D.Render.RenderElement as RenderElement

data SceneManagerData

class SceneManagerInterface a where
    newSceneManagerData :: Renderer.RendererData -> Resources -> IO a
    openSceneManagerData :: a -> Camera.CameraCreateData -> IO ()
    getMainCamera :: a -> IO Camera.CameraObjectData
    addCameraObject :: a -> T.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
    getMainLight :: a -> IO Light.DirectionalLightData
    addDirectionalLightObject :: a -> T.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
    addStaticObject :: a -> T.Text -> StaticObjectCreateData -> IO StaticObjectData
    getStaticObject :: a -> T.Text -> IO (Maybe StaticObjectData)
    getStaticObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
    updateSceneManagerData :: a -> Double -> Float -> IO ()

instance SceneManagerInterface SceneManagerData where
module HulkanEngine3D.Application.SceneManager where

import qualified Data.Text as Text

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
    addCameraObject :: a -> Text.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
    getMainLight :: a -> IO Light.DirectionalLightData
    addDirectionalLightObject :: a -> Text.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
    addRenderObject :: a -> Text.Text -> RenderObjectCreateData -> IO RenderObjectData
    getRenderObject :: a -> Text.Text -> IO (Maybe RenderObjectData)
    getStaticObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
    getSkeletalObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
    updateSceneManagerData :: a -> Double -> Float -> IO ()

instance SceneManagerInterface SceneManagerData where
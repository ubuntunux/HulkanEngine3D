{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NegativeLiterals   #-}

module HulkanEngine3D.Application.SceneManager where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import Data.IORef

import Numeric.DataFrame

import qualified HulkanEngine3D.Render.RenderObject as RenderObject
import qualified HulkanEngine3D.Render.Camera as Camera
import qualified HulkanEngine3D.Render.Light as Light
import qualified HulkanEngine3D.Render.Mesh as Mesh
import qualified HulkanEngine3D.Render.Model as Model
import qualified HulkanEngine3D.Render.RenderElement as RenderElement
import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Render.UniformBufferDatas as UniformBufferDatas
import qualified HulkanEngine3D.Resource.Resource as Resource
--import qualified HulkanEngine3D.Render.TransformObject as TransformObject
import qualified HulkanEngine3D.Utilities.System as System
--import HulkanEngine3D.Utilities.Logger

type CameraObjectMap = HashTable.BasicHashTable Text.Text Camera.CameraObjectData
type DirectionalLightObjectMap = HashTable.BasicHashTable Text.Text Light.DirectionalLightData
type RenderObjectMap = HashTable.BasicHashTable Text.Text RenderObject.RenderObjectData

data SceneManagerData = SceneManagerData
    { _rendererData :: Renderer.RendererData
    , _resources :: Resource.Resources
    , _mainCamera :: IORef Camera.CameraObjectData
    , _mainLight :: IORef Light.DirectionalLightData
    , _cameraObjectMap :: CameraObjectMap
    , _directionalLightObjectMap :: DirectionalLightObjectMap
    , _renderObjectMap :: RenderObjectMap
    , _renderObjectRenderElements :: IORef [RenderElement.RenderElementData]
    } deriving (Show)

class SceneManagerInterface a where
    newSceneManagerData :: Renderer.RendererData -> Resource.Resources -> IO a
    openSceneManagerData :: a -> Camera.CameraCreateData -> IO ()
    getMainCamera :: a -> IO Camera.CameraObjectData
    addCameraObject :: a -> Text.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
    getMainLight :: a -> IO Light.DirectionalLightData
    addDirectionalLightObject :: a -> Text.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
    addRenderObject :: a -> Text.Text -> RenderObject.RenderObjectCreateData -> IO RenderObject.RenderObjectData
    getRenderObject :: a -> Text.Text -> IO (Maybe RenderObject.RenderObjectData)
    getRenderObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
    updateSceneManagerData :: a -> Double -> Float -> IO ()

instance SceneManagerInterface SceneManagerData where
    newSceneManagerData :: Renderer.RendererData -> Resource.Resources -> IO SceneManagerData
    newSceneManagerData rendererData resources = do
        mainCamera <- newIORef (undefined::Camera.CameraObjectData)
        mainLight <- newIORef (undefined::Light.DirectionalLightData)
        cameraObjectMap <- HashTable.new
        directionalLightObjectMap <- HashTable.new
        renderObjectMap <- HashTable.new
        renderObjectRenderElements <- newIORef []
        return SceneManagerData
            { _rendererData = rendererData
            , _resources = resources
            , _mainCamera = mainCamera
            , _mainLight = mainLight
            , _cameraObjectMap = cameraObjectMap
            , _directionalLightObjectMap = directionalLightObjectMap
            , _renderObjectMap = renderObjectMap
            , _renderObjectRenderElements = renderObjectRenderElements
            }

    openSceneManagerData :: SceneManagerData -> Camera.CameraCreateData -> IO ()
    openSceneManagerData sceneManagerData@SceneManagerData {..} cameraCreateData = do
        mainCamera <- addCameraObject sceneManagerData "MainCamera" cameraCreateData
        writeIORef _mainCamera mainCamera

        mainLight <- addDirectionalLightObject sceneManagerData "MainLight" $ Light.defaultDirectionalLightCreateInfo
            { Light._directionalLightPosition' = vec3 0 0 0
            , Light._directionalLightRotation' = vec3 (-3.141592*0.47) 0 0.3
            , Light._directionalLightConstants' = UniformBufferDatas.defaultLightConstants
                { UniformBufferDatas._LIGHT_DIRECTION = vec3 (-3.141592 * 0.47) 0 0.3
                }
            }
        writeIORef _mainLight mainLight

        modelData0 <- Resource.getModelData _resources "sponza/sponza"
        modelData1 <- Resource.getModelData _resources "sphere"
        addRenderObject sceneManagerData "object0" $ RenderObject.defaultRenderObjectCreateData
                    { RenderObject._modelData' = modelData0
                    , RenderObject._position' = vec3 0 0 0
                    , RenderObject._scale' = vec3 0.1 0.1 0.1
                    }
        addRenderObject sceneManagerData "object1" $ RenderObject.defaultRenderObjectCreateData
                    { RenderObject._modelData' = modelData1
                    , RenderObject._position' = vec3 0 1.5 0
                    , RenderObject._scale' = vec3 1.0 1.0 1.0
                    }
        return ()

    getMainCamera :: SceneManagerData -> IO Camera.CameraObjectData
    getMainCamera sceneManagerData = readIORef (_mainCamera sceneManagerData)

    addCameraObject :: SceneManagerData -> Text.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
    addCameraObject sceneManagerData objectName cameraCreateData = do
        newObjectName <- System.generateUniqueName (_cameraObjectMap sceneManagerData) objectName
        cameraObjectData <- Camera.createCameraObjectData newObjectName cameraCreateData
        HashTable.insert (_cameraObjectMap sceneManagerData) newObjectName cameraObjectData
        return cameraObjectData

    getMainLight :: SceneManagerData -> IO Light.DirectionalLightData
    getMainLight sceneManagerData = readIORef (_mainLight sceneManagerData)

    addDirectionalLightObject :: SceneManagerData -> Text.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
    addDirectionalLightObject sceneManagerData objectName lightCreateInfo = do
        newObjectName <- System.generateUniqueName (_directionalLightObjectMap sceneManagerData) objectName
        lightObjectData <- Light.createLightData newObjectName lightCreateInfo
        HashTable.insert (_directionalLightObjectMap sceneManagerData) newObjectName lightObjectData
        return lightObjectData

    addRenderObject :: SceneManagerData -> Text.Text -> RenderObject.RenderObjectCreateData -> IO RenderObject.RenderObjectData
    addRenderObject sceneManagerData objectName renderObjectCreateData = do
        newObjectName <- System.generateUniqueName (_renderObjectMap sceneManagerData) objectName
        renderObjectData <- RenderObject.createRenderObjectData newObjectName renderObjectCreateData
        HashTable.insert (_renderObjectMap sceneManagerData) newObjectName renderObjectData
        return renderObjectData

    getRenderObject :: SceneManagerData -> Text.Text -> IO (Maybe RenderObject.RenderObjectData)
    getRenderObject sceneManagerData objectName = HashTable.lookup (_renderObjectMap sceneManagerData) objectName

    getRenderObjectRenderElements :: SceneManagerData -> IO [RenderElement.RenderElementData]
    getRenderObjectRenderElements sceneManagerData = readIORef (_renderObjectRenderElements sceneManagerData)
    
    updateSceneManagerData :: SceneManagerData -> Double -> Float -> IO ()
    updateSceneManagerData sceneManagerData@SceneManagerData {..} elapsedTime deltaTime = do
        -- update camera & light
        mainCamera <- getMainCamera sceneManagerData
        Camera.updateCameraObjectData mainCamera
        cameraPosition <- Camera.getCameraPosition mainCamera

        mainLight <- getMainLight sceneManagerData
--        TransformObject.rotationYaw (Light._directionalLightTransformObject mainLight) (deltaTime * 0.1)
        Light.updateLightData mainLight cameraPosition

        -- update objects
        flip HashTable.mapM_ _renderObjectMap $ \(objectName, renderObjectData) -> do
            RenderObject.updateRenderObjectData renderObjectData

        -- gather render elements
        writeIORef _renderObjectRenderElements []
        flip HashTable.mapM_ _renderObjectMap $ \(objectName, renderObjectData) -> do
            renderObjectRenderElements <- readIORef _renderObjectRenderElements
            geometryBufferDatas <- readIORef (Mesh._geometryBufferDatas . Model._meshData . RenderObject._modelData $ renderObjectData)
            materialInstanceDatas <- readIORef (Model._materialInstanceDatas . RenderObject._modelData $ renderObjectData)
            let geometryDataCount = length geometryBufferDatas
            renderElementList <- forM [0..(geometryDataCount - 1)] $ \index -> do
                let geometryData = geometryBufferDatas !! index
                    materialInstanceData = materialInstanceDatas !! index
                return RenderElement.RenderElementData
                    { _renderObject = renderObjectData
                    , _geometryData = geometryData
                    , _materialInstanceData = materialInstanceData
                    }
            writeIORef _renderObjectRenderElements (renderObjectRenderElements ++ renderElementList)



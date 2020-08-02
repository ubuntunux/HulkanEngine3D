{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NegativeLiterals   #-}

module HulkanEngine3D.Application.SceneManager where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as T
import Data.IORef

import Numeric.DataFrame

import qualified HulkanEngine3D.Render.RenderObject as RenderObject
import qualified HulkanEngine3D.Render.Camera as Camera
import qualified HulkanEngine3D.Render.Light as Light
import qualified HulkanEngine3D.Render.Mesh as Mesh
import qualified HulkanEngine3D.Render.Model as Model
import qualified HulkanEngine3D.Render.RenderElement as RenderElement
import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Resource.Resource as Resource
--import qualified HulkanEngine3D.Render.TransformObject as TransformObject
import qualified HulkanEngine3D.Utilities.System as System
--import HulkanEngine3D.Utilities.Logger

type CameraObjectMap = HashTable.BasicHashTable T.Text Camera.CameraObjectData
type DirectionalLightObjectMap = HashTable.BasicHashTable T.Text Light.DirectionalLightData
type StaticObjectMap = HashTable.BasicHashTable T.Text RenderObject.StaticObjectData

data SceneManagerData = SceneManagerData
    { _rendererData :: Renderer.RendererData
    , _resources :: Resource.Resources
    , _mainCamera :: IORef Camera.CameraObjectData
    , _mainLight :: IORef Light.DirectionalLightData
    , _cameraObjectMap :: CameraObjectMap
    , _directionalLightObjectMap :: DirectionalLightObjectMap
    , _staticObjectMap :: StaticObjectMap
    , _staticObjectRenderElements :: IORef [RenderElement.RenderElementData]
    } deriving (Show)

class SceneManagerInterface a where
    newSceneManagerData :: Renderer.RendererData -> Resource.Resources -> IO a
    openSceneManagerData :: a -> Camera.CameraCreateData -> IO ()
    getMainCamera :: a -> IO Camera.CameraObjectData
    addCameraObject :: a -> T.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
    getMainLight :: a -> IO Light.DirectionalLightData
    addDirectionalLightObject :: a -> T.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
    addStaticObject :: a -> T.Text -> RenderObject.StaticObjectCreateData -> IO RenderObject.StaticObjectData
    getStaticObject :: a -> T.Text -> IO (Maybe RenderObject.StaticObjectData)
    getStaticObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
    updateSceneManagerData :: a -> Double -> Float -> IO ()

instance SceneManagerInterface SceneManagerData where
    newSceneManagerData :: Renderer.RendererData -> Resource.Resources -> IO SceneManagerData
    newSceneManagerData rendererData resources = do
        mainCamera <- newIORef (undefined::Camera.CameraObjectData)
        mainLight <- newIORef (undefined::Light.DirectionalLightData)
        cameraObjectMap <- HashTable.new
        directionalLightObjectMap <- HashTable.new
        staticObjectMap <- HashTable.new
        staticObjectRenderElements <- newIORef []
        return SceneManagerData
            { _rendererData = rendererData
            , _resources = resources
            , _mainCamera = mainCamera
            , _mainLight = mainLight
            , _cameraObjectMap = cameraObjectMap
            , _directionalLightObjectMap = directionalLightObjectMap
            , _staticObjectMap = staticObjectMap
            , _staticObjectRenderElements = staticObjectRenderElements
            }

    openSceneManagerData :: SceneManagerData -> Camera.CameraCreateData -> IO ()
    openSceneManagerData sceneManagerData@SceneManagerData {..} cameraCreateData = do
        mainCamera <- addCameraObject sceneManagerData "MainCamera" cameraCreateData
        writeIORef _mainCamera mainCamera

        mainLight <- addDirectionalLightObject sceneManagerData "MainLight" $ Light.defaultDirectionalLightCreateInfo
            { Light._directionalLightRotation' = vec3 (-3.141592*0.47) 0 0.3
            }
        writeIORef _mainLight mainLight

        modelData0 <- Resource.getModelData _resources "sponza/sponza"
        modelData1 <- Resource.getModelData _resources "cube"
        addStaticObject sceneManagerData "object0" $ RenderObject.defaultStaticObjectCreateData
                    { RenderObject._modelData' = modelData0
                    , RenderObject._position' = vec3 0 0 0
                    , RenderObject._scale' = vec3 0.1 0.1 0.1
                    }
        addStaticObject sceneManagerData "object1" $ RenderObject.defaultStaticObjectCreateData
                    { RenderObject._modelData' = modelData1
                    , RenderObject._position' = vec3 0 0 0
                    , RenderObject._scale' = vec3 1 1 1
                    }
        return ()

    getMainCamera :: SceneManagerData -> IO Camera.CameraObjectData
    getMainCamera sceneManagerData = readIORef (_mainCamera sceneManagerData)

    addCameraObject :: SceneManagerData -> T.Text -> Camera.CameraCreateData -> IO Camera.CameraObjectData
    addCameraObject sceneManagerData objectName cameraCreateData = do
        newObjectName <- System.generateUniqueName (_cameraObjectMap sceneManagerData) objectName
        cameraObjectData <- Camera.createCameraObjectData newObjectName cameraCreateData
        HashTable.insert (_cameraObjectMap sceneManagerData) newObjectName cameraObjectData
        return cameraObjectData

    getMainLight :: SceneManagerData -> IO Light.DirectionalLightData
    getMainLight sceneManagerData = readIORef (_mainLight sceneManagerData)

    addDirectionalLightObject :: SceneManagerData -> T.Text -> Light.LightCreateInfo -> IO Light.DirectionalLightData
    addDirectionalLightObject sceneManagerData objectName lightCreateData = do
        newObjectName <- System.generateUniqueName (_directionalLightObjectMap sceneManagerData) objectName
        lightObjectData <- Light.createLightData newObjectName lightCreateData
        HashTable.insert (_directionalLightObjectMap sceneManagerData) newObjectName lightObjectData
        return lightObjectData

    addStaticObject :: SceneManagerData -> T.Text -> RenderObject.StaticObjectCreateData -> IO RenderObject.StaticObjectData
    addStaticObject sceneManagerData objectName staticObjectCreateData = do
        newObjectName <- System.generateUniqueName (_staticObjectMap sceneManagerData) objectName
        staticObjectData <- RenderObject.createStaticObjectData newObjectName staticObjectCreateData
        HashTable.insert (_staticObjectMap sceneManagerData) newObjectName staticObjectData
        return staticObjectData

    getStaticObject :: SceneManagerData -> T.Text -> IO (Maybe RenderObject.StaticObjectData)
    getStaticObject sceneManagerData objectName = HashTable.lookup (_staticObjectMap sceneManagerData) objectName

    getStaticObjectRenderElements :: SceneManagerData -> IO [RenderElement.RenderElementData]
    getStaticObjectRenderElements sceneManagerData = readIORef (_staticObjectRenderElements sceneManagerData)
    
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
        flip HashTable.mapM_ _staticObjectMap $ \(objectName, staticObjectData) -> do
            RenderObject.updateStaticObjectData staticObjectData

        -- gather render elements
        writeIORef _staticObjectRenderElements []
        flip HashTable.mapM_ _staticObjectMap $ \(objectName, staticObjectData) -> do
            staticObjectRenderElements <- readIORef _staticObjectRenderElements
            geometryBufferDatas <- readIORef (Mesh._geometryBufferDatas . Model._meshData . RenderObject._modelData $ staticObjectData)
            materialInstanceDatas <- readIORef (Model._materialInstanceDatas . RenderObject._modelData $ staticObjectData)
            let geometryDataCount = length geometryBufferDatas
            renderElementList <- forM [0..(geometryDataCount - 1)] $ \index -> do
                let geometryData = geometryBufferDatas !! index
                    materialInstanceData = materialInstanceDatas !! index
                return RenderElement.RenderElementData
                    { _renderObject = staticObjectData
                    , _geometryData = geometryData
                    , _materialInstanceData = materialInstanceData
                    }
            writeIORef _staticObjectRenderElements (staticObjectRenderElements ++ renderElementList)



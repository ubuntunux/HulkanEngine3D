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
import HulkanEngine3D.Render.Camera
import qualified HulkanEngine3D.Render.Mesh as Mesh
import qualified HulkanEngine3D.Render.Model as Model
import qualified HulkanEngine3D.Render.RenderElement as RenderElement
import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Resource.Resource as Resource
import qualified HulkanEngine3D.Render.TransformObject as TransformObject
import qualified HulkanEngine3D.Utilities.System as System

type CameraObjectMap = HashTable.BasicHashTable T.Text CameraObjectData
type StaticObjectMap = HashTable.BasicHashTable T.Text RenderObject.StaticObjectData

data SceneManagerData = SceneManagerData
    { _rendererData :: Renderer.RendererData
    , _resourceData :: Resource.ResourceData
    , _mainCamera :: IORef CameraObjectData
    , _cameraObjectMap :: CameraObjectMap
    , _staticObjectMap :: StaticObjectMap
    , _staticObjectRenderElements :: IORef [RenderElement.RenderElementData]
    } deriving (Show)

class SceneManagerInterface a where
    newSceneManagerData :: Renderer.RendererData -> Resource.ResourceData -> IO a
    openSceneManagerData :: a -> CameraCreateData -> IO ()
    getMainCamera :: a -> IO CameraObjectData
    addCameraObject :: a -> T.Text -> CameraCreateData -> IO CameraObjectData
    addStaticObject :: a -> T.Text -> RenderObject.StaticObjectCreateData -> IO RenderObject.StaticObjectData
    getStaticObject :: a -> T.Text -> IO (Maybe RenderObject.StaticObjectData)
    getStaticObjectRenderElements :: a -> IO [RenderElement.RenderElementData]
    updateSceneManagerData :: a -> Float -> IO ()

instance SceneManagerInterface SceneManagerData where
    newSceneManagerData :: Renderer.RendererData -> Resource.ResourceData -> IO SceneManagerData
    newSceneManagerData rendererData resourceData = do
        mainCameraRef <- newIORef (undefined::CameraObjectData)
        cameraObjectMap <- HashTable.new
        staticObjectMap <- HashTable.new
        staticObjectRenderElements <- newIORef []
        return SceneManagerData
            { _rendererData = rendererData
            , _resourceData = resourceData
            , _mainCamera = mainCameraRef
            , _cameraObjectMap = cameraObjectMap
            , _staticObjectMap = staticObjectMap
            , _staticObjectRenderElements = staticObjectRenderElements
            }

    openSceneManagerData :: SceneManagerData -> CameraCreateData -> IO ()
    openSceneManagerData sceneManagerData@SceneManagerData {..} cameraCreateData = do
        mainCamera <- addCameraObject sceneManagerData "MainCamera" cameraCreateData
        writeIORef _mainCamera mainCamera

        modelData0 <- Resource.getModelData _resourceData "quad"
        modelData1 <- Resource.getModelData _resourceData "axis_gizmo"
        modelData2 <- Resource.getModelData _resourceData "sphere"

        addStaticObject sceneManagerData "suzan_0" $ RenderObject.StaticObjectCreateData
                    { RenderObject._modelData' = modelData0
                    , RenderObject._position' = vec3 4 0 0
                    }

        addStaticObject sceneManagerData "suzan_0" $ RenderObject.StaticObjectCreateData
                    { RenderObject._modelData' = modelData1
                    , RenderObject._position' = vec3 0 0 0
                    }
        addStaticObject sceneManagerData "suzan_0" $ RenderObject.StaticObjectCreateData
                    { RenderObject._modelData' = modelData2
                    , RenderObject._position' = vec3 -4 0 0
                    }
        return ()

    getMainCamera :: SceneManagerData -> IO CameraObjectData
    getMainCamera sceneManagerData = readIORef (_mainCamera sceneManagerData)

    addCameraObject :: SceneManagerData -> T.Text -> CameraCreateData -> IO CameraObjectData
    addCameraObject sceneManagerData objectName cameraCreateData = do
        newObjectName <- System.generateUniqueName (_cameraObjectMap sceneManagerData) objectName
        cameraObjectData <- createCameraObjectData newObjectName cameraCreateData
        HashTable.insert (_cameraObjectMap sceneManagerData) newObjectName cameraObjectData
        return cameraObjectData

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
    
    updateSceneManagerData :: SceneManagerData -> Float -> IO ()
    updateSceneManagerData sceneManagerData@SceneManagerData {..} deltaTime = do
        -- update camera
        mainCamera <- getMainCamera sceneManagerData
        updateCameraObjectData mainCamera

        -- update objects
        flip HashTable.mapM_ _staticObjectMap $ \(objectName, staticObjectData) -> do
            let transformObjectData = RenderObject.getTransformObjectData staticObjectData
            if ("suzan" == T.unpack objectName) then
                TransformObject.rotationYaw transformObjectData deltaTime
            else do
                TransformObject.rotationPitch transformObjectData (deltaTime * 0.5)
                TransformObject.rotationYaw transformObjectData deltaTime
                TransformObject.rotationRoll transformObjectData (deltaTime * 0.25)
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



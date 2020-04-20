{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Application.SceneManager where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as T
import Data.IORef
import qualified Data.Vector.Mutable as MVector

import Numeric.DataFrame

import qualified HulkanEngine3D.Render.RenderObject as RenderObject
import HulkanEngine3D.Render.Camera
import qualified HulkanEngine3D.Render.Mesh as Mesh
import qualified HulkanEngine3D.Render.Model as Model
import qualified HulkanEngine3D.Render.RenderElement as RenderElement
import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Resource.Resource as Resource

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
    updateSceneManagerData :: a -> IO ()

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

        Just modelData <- Resource.getModelData _resourceData "suzan"
        let staticObjectCreateData = RenderObject.StaticObjectCreateData
                { RenderObject._modelData' = modelData
                , RenderObject._position' = vec3 0 0 0
                }
        addStaticObject sceneManagerData "suzan" staticObjectCreateData
        return ()

    getMainCamera :: SceneManagerData -> IO CameraObjectData
    getMainCamera sceneManagerData = readIORef (_mainCamera sceneManagerData)

    addCameraObject :: SceneManagerData -> T.Text -> CameraCreateData -> IO CameraObjectData
    addCameraObject sceneManagerData objectName cameraCreateData = do
        newObjectName <- generateObjectName (_cameraObjectMap sceneManagerData) objectName
        cameraObjectData <- createCameraObjectData newObjectName cameraCreateData
        HashTable.insert (_cameraObjectMap sceneManagerData) newObjectName cameraObjectData
        return cameraObjectData

    addStaticObject :: SceneManagerData -> T.Text -> RenderObject.StaticObjectCreateData -> IO RenderObject.StaticObjectData
    addStaticObject sceneManagerData objectName staticObjectCreateData = do
        newObjectName <- generateObjectName (_staticObjectMap sceneManagerData) objectName
        staticObjectData <- RenderObject.createStaticObjectData newObjectName staticObjectCreateData
        HashTable.insert (_staticObjectMap sceneManagerData) newObjectName staticObjectData
        return staticObjectData

    getStaticObject :: SceneManagerData -> T.Text -> IO (Maybe RenderObject.StaticObjectData)
    getStaticObject sceneManagerData objectName = HashTable.lookup (_staticObjectMap sceneManagerData) objectName
    
    updateSceneManagerData :: SceneManagerData -> IO ()
    updateSceneManagerData sceneManagerData@SceneManagerData {..} = do
        mainCamera <- getMainCamera sceneManagerData
        updateCameraObjectData mainCamera

        flip HashTable.mapM_ _staticObjectMap $ \(_, staticObjectData) ->
            RenderObject.updateStaticObjectData staticObjectData

        writeIORef _staticObjectRenderElements []
        flip HashTable.mapM_ _staticObjectMap $ \(_, staticObjectData) -> do
            staticObjectRenderElements <- readIORef _staticObjectRenderElements
            geometryBufferDatas <- readIORef (Mesh._geometryBufferDatas . Model._meshData . RenderObject._modelData $ staticObjectData)
            materialInstanceDatas <- readIORef (Model._materialInstanceDatas . RenderObject._modelData $ staticObjectData)
            let geometryDataCount = MVector.length geometryBufferDatas
            renderElementList <- forM [0..(geometryDataCount-1)] $ \index -> do
                geometryData <- MVector.read geometryBufferDatas index
                materialInstanceData <- MVector.read materialInstanceDatas index
                return RenderElement.RenderElementData
                    { _renderObject = staticObjectData
                    , _geometryData = geometryData
                    , _materialInstanceData = materialInstanceData
                    }
            writeIORef _staticObjectRenderElements (staticObjectRenderElements ++ renderElementList)

generateObjectName :: HashTable.BasicHashTable T.Text v -> T.Text -> IO T.Text
generateObjectName objectMap objectName = do
    objectData <- HashTable.lookup objectMap objectName
    case objectData of
        Nothing -> return objectName
        otherwise -> generator objectMap objectName (0::Int)
    where
        generator sceneManagerData objectName index = do
            objectData <- HashTable.lookup objectMap objectName
            case objectData of
                Nothing -> pure $ T.append objectName $ T.append (T.pack "_") (T.pack . show $ index)
                otherwise -> generator objectMap objectName (index + 1)

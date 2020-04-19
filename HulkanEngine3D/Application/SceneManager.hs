{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Application.SceneManager where

import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as T
import Data.IORef

import Numeric.DataFrame

import HulkanEngine3D.Render.Actor
import HulkanEngine3D.Render.Camera
import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Resource.Resource as Resource

type CameraObjectMap = HashTable.BasicHashTable T.Text CameraObjectData
type StaticObjectMap = HashTable.BasicHashTable T.Text StaticObjectData

data SceneManagerData = SceneManagerData
    { _rendererData :: Renderer.RendererData
    , _resourceData :: Resource.ResourceData
    , _mainCamera :: IORef CameraObjectData
    , _cameraObjectMap :: CameraObjectMap
    , _staticObjectMap :: StaticObjectMap
    } deriving (Show)

class SceneManagerInterface a where
    newSceneManagerData :: Renderer.RendererData -> Resource.ResourceData -> IO a
    openSceneManagerData :: a -> CameraCreateData -> IO ()
    getMainCamera :: a -> IO CameraObjectData
    addCameraObject :: a -> T.Text -> CameraCreateData -> IO CameraObjectData
    addStaticObject :: a -> T.Text -> StaticObjectCreateData -> IO StaticObjectData
    getStaticObject :: a -> T.Text -> IO (Maybe StaticObjectData)
    updateSceneManagerData :: a -> IO ()

instance SceneManagerInterface SceneManagerData where
    newSceneManagerData :: Renderer.RendererData -> Resource.ResourceData -> IO SceneManagerData
    newSceneManagerData rendererData resourceData = do
        mainCameraRef <- newIORef (undefined::CameraObjectData)
        cameraObjectMap <- HashTable.new
        staticObjectMap <- HashTable.new
        return SceneManagerData
            { _rendererData = rendererData
            , _resourceData = resourceData
            , _mainCamera = mainCameraRef
            , _cameraObjectMap = cameraObjectMap
            , _staticObjectMap = staticObjectMap
            }

    openSceneManagerData :: SceneManagerData -> CameraCreateData -> IO ()
    openSceneManagerData sceneManagerData@SceneManagerData {..} cameraCreateData = do
        mainCamera <- addCameraObject sceneManagerData "MainCamera" cameraCreateData
        writeIORef _mainCamera mainCamera

        Just modelData <- Resource.getModelData _resourceData "suzan"
        let staticObjectCreateData = StaticObjectCreateData
                { _modelData' = modelData
                , _position' = vec3 0 0 0
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

    addStaticObject :: SceneManagerData -> T.Text -> StaticObjectCreateData -> IO StaticObjectData
    addStaticObject sceneManagerData objectName staticObjectCreateData = do
        newObjectName <- generateObjectName (_staticObjectMap sceneManagerData) objectName
        staticObjectData <- createStaticObjectData newObjectName staticObjectCreateData
        HashTable.insert (_staticObjectMap sceneManagerData) newObjectName staticObjectData
        return staticObjectData

    getStaticObject :: SceneManagerData -> T.Text -> IO (Maybe StaticObjectData)
    getStaticObject sceneManagerData objectName = HashTable.lookup (_staticObjectMap sceneManagerData) objectName
    
    updateSceneManagerData :: SceneManagerData -> IO ()
    updateSceneManagerData sceneManagerData = do
        mainCamera <- getMainCamera sceneManagerData
        updateCameraObjectData mainCamera

        HashTable.mapM_ (\(k, v) -> updateStaticObjectData v) (_staticObjectMap sceneManagerData)
    

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

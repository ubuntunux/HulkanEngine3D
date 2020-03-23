{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Application.SceneManager where

import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as T
import Data.IORef

import HulkanEngine3D.Render.Actor
import HulkanEngine3D.Render.Camera


type CameraObjectMap = HashTable.BasicHashTable T.Text CameraObjectData
type StaticObjectMap = HashTable.BasicHashTable T.Text StaticObjectData

data SceneManagerData = SceneManagerData
    { _mainCamera :: IORef CameraObjectData
    , _cameraObjectMap :: CameraObjectMap
    , _staticObjectMap :: StaticObjectMap
    } deriving (Show)


newSceneManagerData :: IO SceneManagerData
newSceneManagerData = do
    mainCameraRef <- newIORef (undefined::CameraObjectData)
    cameraObjectMap <- HashTable.new
    staticObjectMap <- HashTable.new
    return SceneManagerData
        { _mainCamera = mainCameraRef
        , _cameraObjectMap = cameraObjectMap
        , _staticObjectMap = staticObjectMap
        }

initializeSceneManagerData :: SceneManagerData -> CameraCreateData -> IO ()
initializeSceneManagerData sceneManagerData cameraCreateData = do
    mainCamera <- addCameraObject sceneManagerData "MainCamera" cameraCreateData
    writeIORef (_mainCamera sceneManagerData) mainCamera

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

getObject :: HashTable.BasicHashTable T.Text v -> T.Text -> IO (Maybe v)
getObject objectMap objectName = HashTable.lookup objectMap objectName

addCameraObject :: SceneManagerData -> T.Text -> CameraCreateData -> IO CameraObjectData
addCameraObject sceneManagerData objectName cameraCreateData = do
    objectName <- generateObjectName (_cameraObjectMap sceneManagerData) objectName
    cameraObjectData <- createCameraObjectData objectName cameraCreateData
    HashTable.insert (_cameraObjectMap sceneManagerData) objectName cameraObjectData
    return cameraObjectData

updateSceneManagerData :: SceneManagerData -> SceneManagerData
updateSceneManagerData sceneManagerData = sceneManagerData


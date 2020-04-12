{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Application.SceneManager where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as T
import Data.IORef

import HulkanEngine3D.Render.Actor
import HulkanEngine3D.Render.Camera
import qualified HulkanEngine3D.Render.Renderer as Renderer
import HulkanEngine3D.Render.RenderElement
import HulkanEngine3D.Render.UniformBufferDatas
import HulkanEngine3D.Resource.Resource
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.UniformBuffer
import HulkanEngine3D.Vulkan.Texture

type CameraObjectMap = HashTable.BasicHashTable T.Text CameraObjectData
type StaticObjectMap = HashTable.BasicHashTable T.Text StaticObjectData

data SceneManagerData = SceneManagerData
    { _rendererData :: Renderer.RendererData
    , _resourceData :: ResourceData
    , _mainCamera :: IORef CameraObjectData
    , _cameraObjectMap :: CameraObjectMap
    , _staticObjectMap :: StaticObjectMap
    , _renderElement :: IORef RenderElementData
    } deriving (Show)

class SceneManagerInterface a where
    newSceneManagerData :: Renderer.RendererData -> ResourceData -> IO a
    loadSceneManagerData :: a -> CameraCreateData -> IO ()
    getMainCamera :: a -> IO CameraObjectData
    addCameraObject :: a -> T.Text -> CameraCreateData -> IO CameraObjectData
    updateSceneManagerData :: a -> IO ()

instance SceneManagerInterface SceneManagerData where
    newSceneManagerData :: Renderer.RendererData -> ResourceData -> IO SceneManagerData
    newSceneManagerData rendererData resourceData = do
        mainCameraRef <- newIORef (undefined::CameraObjectData)
        cameraObjectMap <- HashTable.new
        staticObjectMap <- HashTable.new
        renderElementRef <- newIORef defaultRenderElementData
        return SceneManagerData
            { _rendererData = rendererData
            , _resourceData = resourceData
            , _mainCamera = mainCameraRef
            , _cameraObjectMap = cameraObjectMap
            , _staticObjectMap = staticObjectMap
            , _renderElement = renderElementRef
            }

    loadSceneManagerData :: SceneManagerData -> CameraCreateData -> IO ()
    loadSceneManagerData sceneManagerData@SceneManagerData {..} cameraCreateData = do
        Just defaultRenderPassData <- getDefaultRenderPassData _resourceData
        Just pipeline <- getPipelineData defaultRenderPassData "RenderTriangle"
        let descriptorData = _descriptorData pipeline

        descriptorSets <- createDescriptorSet (Renderer.getDevice _rendererData) descriptorData
        let renderElement = RenderElementData { _descriptorSets = descriptorSets }
        _renderElement <- writeIORef _renderElement renderElement

        Just textureData <- getTextureData _resourceData "texture"

        let descriptorBufferInfos = _descriptorBufferInfos . _sceneConstantsBufferData $ (Renderer._uniformBufferDatas _rendererData)
            descriptorImageInfo = _descriptorImageInfo textureData

        forM_ (zip descriptorBufferInfos descriptorSets) $ \(descriptorBufferInfo, descriptorSet) -> do
            let descriptorBufferOrImageInfos = [Left descriptorBufferInfo, Right descriptorImageInfo]::[DescriptorBufferOrImageInfo]
            updateDescriptorSets (Renderer.getDevice _rendererData) descriptorSet (_descriptorSetLayoutBindingList descriptorData) descriptorBufferOrImageInfos

        mainCamera <- addCameraObject sceneManagerData "MainCamera" cameraCreateData
        writeIORef _mainCamera mainCamera

    getMainCamera :: SceneManagerData -> IO CameraObjectData
    getMainCamera sceneManagerData = readIORef (_mainCamera sceneManagerData)

    addCameraObject :: SceneManagerData -> T.Text -> CameraCreateData -> IO CameraObjectData
    addCameraObject sceneManagerData objectName cameraCreateData = do
        objectName <- generateObjectName (_cameraObjectMap sceneManagerData) objectName
        cameraObjectData <- createCameraObjectData objectName cameraCreateData
        HashTable.insert (_cameraObjectMap sceneManagerData) objectName cameraObjectData
        return cameraObjectData
    
    updateSceneManagerData :: SceneManagerData -> IO ()
    updateSceneManagerData sceneManagerData = do
        mainCamera <- getMainCamera sceneManagerData
        updateCameraObjectData mainCamera
    

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
                    
getSceneObject :: HashTable.BasicHashTable T.Text v -> T.Text -> IO (Maybe v)
getSceneObject objectMap objectName = HashTable.lookup objectMap objectName




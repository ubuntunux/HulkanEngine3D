{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.Resource
    ( ResourceData (..)
    , ResourceInterface (..)
    ) where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import System.FilePath.Posix

import qualified HulkanEngine3D.Constants as Constants
import {-# SOURCE #-} HulkanEngine3D.Application.SceneManager
import HulkanEngine3D.Render.Mesh
import qualified HulkanEngine3D.Render.Model as Model
import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.UniformBufferDatas
import HulkanEngine3D.Resource.ObjLoader
import qualified HulkanEngine3D.Resource.MaterialInstanceCreateInfo as MaterialInstanceCreateInfo
import HulkanEngine3D.Resource.RenderPassCreateInfo
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.UniformBuffer
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System


type FrameBufferDataMap = HashTable.BasicHashTable Text.Text FrameBufferData
type MaterialInstanceDataMap = HashTable.BasicHashTable Text.Text MaterialInstanceData
type SceneManagerDataMap = HashTable.BasicHashTable Text.Text SceneManagerData
type MeshDataMap = HashTable.BasicHashTable Text.Text MeshData
type ModelDataMap = HashTable.BasicHashTable Text.Text Model.ModelData
type TextureDataMap = HashTable.BasicHashTable Text.Text TextureData
type RenderPassDataMap = HashTable.BasicHashTable Text.Text RenderPassData
type DescriptorDataMap = HashTable.BasicHashTable Text.Text DescriptorData

gatherAllFiles :: Bool
gatherAllFiles = False

meshFilePath :: FilePath
meshFilePath = "Resource/Externals/Meshes"

textureFilePath :: FilePath
textureFilePath = "Resource/Externals/Textures"

data ResourceData = ResourceData
    { _meshDataMap :: MeshDataMap
    , _modelDataMap :: ModelDataMap
    , _textureDataMap :: TextureDataMap
    , _frameBufferDataMap :: FrameBufferDataMap
    , _renderPassDataMap :: RenderPassDataMap
    , _materialInstanceDataMap :: MaterialInstanceDataMap
    , _descriptorDataMap :: DescriptorDataMap
    } deriving (Show)


class ResourceInterface a where
    createNewResourceData :: IO a
    initializeResourceData :: a -> RendererData -> IO ()
    destroyResourceData :: a -> RendererData -> IO ()

    loadGraphicsDatas :: a -> RendererData -> IO ()
    unloadGraphicsDatas :: a -> RendererData -> IO ()

    loadSceneManagerDatas :: a -> RendererData -> IO ()
    unloadSceneManagerDatas :: a -> RendererData -> IO ()

    loadModelDatas :: a -> RendererData -> IO ()
    unloadModelDatas :: a -> RendererData -> IO ()
    getModelData :: a -> Text.Text -> IO (Maybe Model.ModelData)

    loadMeshDatas :: a -> RendererData -> IO ()
    unloadMeshDatas :: a -> RendererData -> IO ()
    getMeshData :: a -> Text.Text -> IO (Maybe MeshData)

    loadTextureDatas :: a -> RendererData -> IO ()
    unloadTextureDatas :: a -> RendererData -> IO ()
    getTextureData :: a -> Text.Text -> IO (Maybe TextureData)

    loadFrameBufferDatas :: a -> RendererData -> IO ()
    unloadFrameBufferDatas :: a -> RendererData -> IO ()
    getFrameBufferData :: a -> Text.Text -> IO (Maybe FrameBufferData)

    loadRenderPassDatas :: a -> RendererData -> IO ()
    unloadRenderPassDatas :: a -> RendererData -> IO ()
    getRenderPassData :: a -> Text.Text -> IO (Maybe RenderPassData)
    getDefaultRenderPassData :: a -> IO (Maybe RenderPassData)

    loadMaterialInstanceDatas :: a -> RendererData -> IO ()
    unloadMaterialInstanceDatas :: a -> RendererData -> IO ()
    getMaterialInstanceData :: a -> Text.Text -> IO (Maybe MaterialInstanceData)
    getDefaultMaterialInstanceData :: a -> IO (Maybe MaterialInstanceData)

    getDescriptorData :: a -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO DescriptorData
    unloadDescriptorDatas :: a -> RendererData -> IO ()

instance ResourceInterface ResourceData where
    createNewResourceData :: IO ResourceData
    createNewResourceData = do
        frameBufferDataMap <- HashTable.new
        modelDataMap <- HashTable.new
        meshDataMap <- HashTable.new
        textureDataMap <- HashTable.new
        renderPassDataMap <- HashTable.new
        materialInstanceDataMap <- HashTable.new
        descriptorDataMap <- HashTable.new
        return ResourceData
            { _frameBufferDataMap = frameBufferDataMap
            , _modelDataMap = modelDataMap
            , _meshDataMap = meshDataMap
            , _textureDataMap = textureDataMap
            , _renderPassDataMap = renderPassDataMap
            , _materialInstanceDataMap = materialInstanceDataMap
            , _descriptorDataMap = descriptorDataMap
            }

    initializeResourceData :: ResourceData -> RendererData -> IO ()
    initializeResourceData resourceData rendererData = do
        logInfo "initializeResourceData"
        loadTextureDatas resourceData rendererData
        loadRenderPassDatas resourceData rendererData
        loadFrameBufferDatas resourceData rendererData
        loadMaterialInstanceDatas resourceData rendererData
        loadMeshDatas resourceData rendererData
        loadModelDatas resourceData rendererData

    destroyResourceData :: ResourceData -> RendererData -> IO ()
    destroyResourceData resourceData rendererData = do
        logInfo "destroyResourceData"
        unloadModelDatas resourceData rendererData
        unloadMeshDatas resourceData rendererData
        unloadMaterialInstanceDatas resourceData rendererData
        unloadFrameBufferDatas resourceData rendererData
        unloadRenderPassDatas resourceData rendererData
        unloadTextureDatas resourceData rendererData
        unloadDescriptorDatas resourceData rendererData

    -- GraphicsDatas
    loadGraphicsDatas :: ResourceData -> RendererData -> IO ()
    loadGraphicsDatas resourceData rendererData = do
        loadFrameBufferDatas resourceData rendererData

    unloadGraphicsDatas :: ResourceData -> RendererData -> IO ()
    unloadGraphicsDatas resourceData rendererData = do
        unloadFrameBufferDatas resourceData rendererData

    -- SceneManagerData
    loadSceneManagerDatas :: ResourceData -> RendererData -> IO ()
    loadSceneManagerDatas resourceData rendererData = return ()

    unloadSceneManagerDatas :: ResourceData -> RendererData -> IO ()
    unloadSceneManagerDatas resourceData rendererData = return ()

    -- Model Loader
    loadModelDatas :: ResourceData -> RendererData -> IO ()
    loadModelDatas resourceData rendererData = do
        flip HashTable.mapM_ (_meshDataMap resourceData) $ \(meshName, meshData) -> do
            Just materialInstanceData <- getDefaultMaterialInstanceData resourceData
            let modelName = meshName
            geometryBufferDataCount <- getGeometryDataCount meshData
            let materialInstances = replicate geometryBufferDataCount materialInstanceData
            modelData <- Model.newModelData modelName meshData materialInstances
            HashTable.insert (_modelDataMap resourceData) modelName modelData

    unloadModelDatas :: ResourceData -> RendererData -> IO ()
    unloadModelDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> Model.destroyModelData v) (_modelDataMap resourceData)

    getModelData :: ResourceData -> Text.Text -> IO (Maybe Model.ModelData)
    getModelData resourceData resourceName =
        HashTable.lookup (_modelDataMap resourceData) resourceName

    -- Mesh Loader
    loadMeshDatas :: ResourceData -> RendererData -> IO ()
    loadMeshDatas resourceData rendererData = do
        meshFiles <- if gatherAllFiles then
                walkDirectory meshFilePath [".obj"]
            else
                return ["Resource/Externals/Meshes/suzan.obj"]
        forM_ meshFiles $ \meshFile -> do
            let meshName = Text.pack $ drop (length meshFilePath + 1) (dropExtension meshFile)
            (vertices, indices) <- loadModel meshFile
            geometryBufferData <- createGeometryBuffer rendererData meshName vertices indices
            meshData <- newMeshData meshName [geometryBufferData]
            HashTable.insert (_meshDataMap resourceData) meshName meshData

    unloadMeshDatas :: ResourceData -> RendererData -> IO ()
    unloadMeshDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> (action rendererData k v)) (_meshDataMap resourceData)
        where
            action rendererData name meshData = do
                geometryDataCount <- getGeometryDataCount meshData
                forM_  [0..(geometryDataCount-1)] $ \index -> do
                    geometryData <- getGeometryData meshData index
                    destroyGeometryBuffer rendererData geometryData

    getMeshData :: ResourceData -> Text.Text -> IO (Maybe MeshData)
    getMeshData resourceData resourceName =
        HashTable.lookup (_meshDataMap resourceData) resourceName

    -- TextureLoader
    loadTextureDatas :: ResourceData -> RendererData -> IO ()
    loadTextureDatas resourceData rendererData = do
        textureFiles <- if gatherAllFiles then
                walkDirectory textureFilePath [".jpg", ".png"]
            else
                return ["Resource/Externals/Textures/texture.jpg"]
        forM_ textureFiles $ \textureFile -> do
            let textureDataName = Text.pack $ drop (length textureFilePath + 1) (dropExtension textureFile)
            textureData <- createTexture rendererData textureDataName textureFile
            HashTable.insert (_textureDataMap resourceData) textureDataName textureData

    unloadTextureDatas :: ResourceData -> RendererData -> IO ()
    unloadTextureDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyTexture rendererData v) (_textureDataMap resourceData)

    getTextureData :: ResourceData -> Text.Text -> IO (Maybe TextureData)
    getTextureData resourceData resourceName =
        HashTable.lookup (_textureDataMap resourceData) resourceName

    -- FrameBuffer
    loadFrameBufferDatas :: ResourceData -> RendererData -> IO ()
    loadFrameBufferDatas resourceData rendererData = do
        defaultRenderPassDataCreateInfo <- getRenderPassDataCreateInfo rendererData
        let frameBufferDataCreateInfo = _frameBufferDataCreateInfo defaultRenderPassDataCreateInfo
        Just defaultRenderPassData <- getRenderPassData resourceData (_renderPassCreateInfoName defaultRenderPassDataCreateInfo)
        defaultFrameBufferData <- createFrameBufferData (getDevice rendererData) (_renderPass defaultRenderPassData) frameBufferDataCreateInfo
        HashTable.insert (_frameBufferDataMap resourceData) (_frameBufferName frameBufferDataCreateInfo) defaultFrameBufferData

    unloadFrameBufferDatas :: ResourceData -> RendererData -> IO ()
    unloadFrameBufferDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyFrameBufferData (getDevice rendererData) v) (_frameBufferDataMap resourceData)

    getFrameBufferData :: ResourceData -> Text.Text -> IO (Maybe FrameBufferData)
    getFrameBufferData resourceData resourceName =
       HashTable.lookup (_frameBufferDataMap resourceData) resourceName

    -- RenderPassLoader
    loadRenderPassDatas :: ResourceData -> RendererData -> IO ()
    loadRenderPassDatas resourceData rendererData = do
        defaultRenderPassDataCreateInfo <- getRenderPassDataCreateInfo rendererData
        descriptorDatas <- forM (_pipelineDataCreateInfos defaultRenderPassDataCreateInfo) $ \pipelineDataCreateInfo ->
            getDescriptorData resourceData rendererData (_renderPassCreateInfoName defaultRenderPassDataCreateInfo) pipelineDataCreateInfo
        defaultRenderPassData <- createRenderPassData (getDevice rendererData) defaultRenderPassDataCreateInfo descriptorDatas
        HashTable.insert (_renderPassDataMap resourceData) (_renderPassDataName defaultRenderPassData) defaultRenderPassData

    unloadRenderPassDatas :: ResourceData -> RendererData -> IO ()
    unloadRenderPassDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyRenderPassData (getDevice rendererData) v) (_renderPassDataMap resourceData)

    getRenderPassData :: ResourceData -> Text.Text -> IO (Maybe RenderPassData)
    getRenderPassData resourceData resourceName =
        HashTable.lookup (_renderPassDataMap resourceData) resourceName

    getDefaultRenderPassData :: ResourceData -> IO (Maybe RenderPassData)
    getDefaultRenderPassData resourceData =
        getRenderPassData resourceData "defaultRenderPass"

    -- MaterialInstanceDatas
    loadMaterialInstanceDatas :: ResourceData -> RendererData -> IO ()
    loadMaterialInstanceDatas resourceData rendererData = do
        Just renderPassData <- getRenderPassData resourceData "defaultRenderPass"
        Just pipelineData <- getPipelineData renderPassData "RenderTriangle"
        Just textureData <- getTextureData resourceData "texture"

        let descriptorBufferInfos = _descriptorBufferInfos . _sceneConstantsBufferData . _uniformBufferDatas $ rendererData
            descriptorImageInfo = _descriptorImageInfo textureData

        descriptorBufferOrImageInfosList <- forM descriptorBufferInfos $ \descriptorBufferInfo ->
            return [DescriptorBufferInfo descriptorBufferInfo, DescriptorImageInfo descriptorImageInfo]
        let materialInstanceCreateInfo = MaterialInstanceCreateInfo.MaterialInstanceCreateInfo
                { MaterialInstanceCreateInfo._renderPassData = renderPassData
                , MaterialInstanceCreateInfo._pipelineData = pipelineData
                , MaterialInstanceCreateInfo._descriptorBufferOrImageInfosList = descriptorBufferOrImageInfosList
                }
        materialInstance <- createMaterialInstance (getDevice rendererData) materialInstanceCreateInfo
        HashTable.insert (_materialInstanceDataMap resourceData) "default" materialInstance

    unloadMaterialInstanceDatas :: ResourceData -> RendererData -> IO ()
    unloadMaterialInstanceDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyMaterialInstance (getDevice rendererData) v) (_materialInstanceDataMap resourceData)

    getMaterialInstanceData :: ResourceData -> Text.Text -> IO (Maybe MaterialInstanceData)
    getMaterialInstanceData resourceData resourceName =
        HashTable.lookup (_materialInstanceDataMap resourceData) resourceName

    getDefaultMaterialInstanceData :: ResourceData -> IO (Maybe MaterialInstanceData)
    getDefaultMaterialInstanceData resourceData = getMaterialInstanceData resourceData "default"

    -- DescriptorDatas
    getDescriptorData :: ResourceData -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO DescriptorData
    getDescriptorData resourceData rendererData renderPassName pipelineDataCreateInfo = do
        let descriptorName = Text.append renderPassName (_pipelineDataCreateInfoName pipelineDataCreateInfo)
            descriptorDataCreateInfoList = _descriptorDataCreateInfoList pipelineDataCreateInfo
            descriptorCount = Constants.swapChainImageCount
        maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resourceData) descriptorName
        case maybeDescriptorData of
            (Just descriptorData) -> return descriptorData
            otherwise -> do
                descriptorData <- createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList descriptorCount
                HashTable.insert (_descriptorDataMap resourceData) descriptorName descriptorData
                return descriptorData

    unloadDescriptorDatas :: ResourceData -> RendererData -> IO ()
    unloadDescriptorDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyDescriptorData (getDevice rendererData) v) (_descriptorDataMap resourceData)
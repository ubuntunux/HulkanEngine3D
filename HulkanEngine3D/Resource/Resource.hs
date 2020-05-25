{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module HulkanEngine3D.Resource.Resource
    ( ResourceData (..)
    , ResourceInterface (..)
    ) where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.FilePath.Posix
import qualified Data.Aeson as Aeson
--import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap

import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Constants as Constants
import {-# SOURCE #-} HulkanEngine3D.Application.SceneManager
import HulkanEngine3D.Render.Mesh
import qualified HulkanEngine3D.Render.Model as Model
import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Resource.ObjLoader
import qualified HulkanEngine3D.Resource.FrameBufferCreateInfo as FrameBufferCreateInfo
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo as RenderPassCreateInfo
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import qualified HulkanEngine3D.Vulkan.GeometryBuffer as GeometryBuffer
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.UniformBuffer
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System


type ResourceDataMap a = HashTable.BasicHashTable Text.Text a
type FrameBufferDataMap = ResourceDataMap FrameBufferData
type MaterialInstanceDataMap = ResourceDataMap MaterialInstanceData
type SceneManagerDataMap = ResourceDataMap SceneManagerData
type MeshDataMap = ResourceDataMap MeshData
type ModelDataMap = ResourceDataMap Model.ModelData
type TextureDataMap = ResourceDataMap TextureData
type RenderPassDataMap = ResourceDataMap RenderPassData
type DescriptorDataMap = ResourceDataMap Descriptor.DescriptorData

gatherAllFiles :: Bool
gatherAllFiles = False

materialInstanceFilePath :: FilePath
materialInstanceFilePath = "Resource/MaterialInstances"

meshFilePath :: FilePath
meshFilePath = "Resource/Externals/Meshes"

modelFilePath :: FilePath
modelFilePath = "Resource/Models"

textureFilePath :: FilePath
textureFilePath = "Resource/Externals/Textures"

defaultMeshName :: Text.Text
defaultMeshName = "quad"

defaultModelName :: Text.Text
defaultModelName = "quad"

defaultTextureName :: Text.Text
defaultTextureName = "common/default"

defaultMaterialInstanceName :: Text.Text
defaultMaterialInstanceName = "render_default"

defaultFrameBufferName :: Text.Text
defaultFrameBufferName = "render_default"

defaultRenderPassName :: Text.Text
defaultRenderPassName = "render_default"


data ResourceData = ResourceData
    { _meshDataMap :: MeshDataMap
    , _modelDataMap :: ModelDataMap
    , _textureDataMap :: TextureDataMap
    , _frameBufferDataMap :: FrameBufferDataMap
    , _renderPassDataMap :: RenderPassDataMap
    , _materialInstanceDataMap :: MaterialInstanceDataMap
    , _descriptorDataMap :: DescriptorDataMap
    } deriving (Show)


getResourceData :: ResourceDataMap r -> Text.Text -> Text.Text -> IO r
getResourceData resourceDataMap resourceName defaultResourceName = do
    maybeData <- HashTable.lookup resourceDataMap resourceName
    case maybeData of
        Nothing -> getDefaultResourceData
        otherwise -> return (Maybe.fromJust maybeData)
    where
        getDefaultResourceData = Maybe.fromJust <$> HashTable.lookup resourceDataMap defaultResourceName

getResourceNameFromFilepath :: ResourceDataMap r -> FilePath -> FilePath -> IO Text.Text
getResourceNameFromFilepath resourceDataMap resourcePath resourceFilePath = do
    let resourceName = Text.pack $ drop (length resourcePath + 1) (dropExtension resourceFilePath)
    generateUniqueName resourceDataMap resourceName


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
    getModelData :: a -> Text.Text -> IO Model.ModelData

    loadMeshDatas :: a -> RendererData -> IO ()
    unloadMeshDatas :: a -> RendererData -> IO ()
    getMeshData :: a -> Text.Text -> IO MeshData

    loadTextureDatas :: a -> RendererData -> IO ()
    unloadTextureDatas :: a -> RendererData -> IO ()
    getTextureData :: a -> Text.Text -> IO TextureData

    loadFrameBufferDatas :: a -> RendererData -> IO ()
    unloadFrameBufferDatas :: a -> RendererData -> IO ()
    getFrameBufferData :: a -> Text.Text -> IO (Maybe FrameBufferData)

    loadRenderPassDatas :: a -> RendererData -> IO ()
    unloadRenderPassDatas :: a -> RendererData -> IO ()
    getRenderPassData :: a -> Text.Text -> IO (Maybe RenderPassData)
    getDefaultRenderPassData :: a -> IO (Maybe RenderPassData)

    loadMaterialInstanceDatas :: a -> RendererData -> IO ()
    unloadMaterialInstanceDatas :: a -> RendererData -> IO ()
    reloadMaterialInstanceDatas :: a -> RendererData -> IO ()
    getMaterialInstanceData :: a -> Text.Text -> IO MaterialInstanceData

    getDescriptorData :: a -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO Descriptor.DescriptorData
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
        logInfo "ResourceData::loadGraphicsDatas"
        loadRenderPassDatas resourceData rendererData
        loadFrameBufferDatas resourceData rendererData
        loadMaterialInstanceDatas resourceData rendererData
        reloadMaterialInstanceDatas resourceData rendererData

    unloadGraphicsDatas :: ResourceData -> RendererData -> IO ()
    unloadGraphicsDatas resourceData rendererData = do
        logInfo "ResourceData::unloadGraphicsDatas"
        unloadMaterialInstanceDatas resourceData rendererData
        unloadFrameBufferDatas resourceData rendererData
        unloadRenderPassDatas resourceData rendererData
        unloadDescriptorDatas resourceData rendererData

    -- SceneManagerData
    loadSceneManagerDatas :: ResourceData -> RendererData -> IO ()
    loadSceneManagerDatas resourceData rendererData = return ()

    unloadSceneManagerDatas :: ResourceData -> RendererData -> IO ()
    unloadSceneManagerDatas resourceData rendererData = return ()

    -- Model Loader
    loadModelDatas :: ResourceData -> RendererData -> IO ()
    loadModelDatas resourceData rendererData = do
        modelFiles <- walkDirectory modelFilePath [".model"]
        forM_ modelFiles $ \modelFile -> do
            modelName <- getResourceNameFromFilepath (_modelDataMap resourceData) modelFilePath modelFile
            contents <- ByteString.readFile modelFile
            registModelData (_modelDataMap resourceData) modelName contents
        where
            registModelData modelDataMap modelName contents = do
                let Just (Aeson.Object modelCreateInfoMap) = Aeson.decodeStrict contents
                    Just (Aeson.Array materialInstanceNames) = HashMap.lookup "material_instances" modelCreateInfoMap
                    Just (Aeson.String meshName) = HashMap.lookup "mesh" modelCreateInfoMap
                materialInstanceDatas <- forM (Vector.toList materialInstanceNames) $ \(Aeson.String materialInstanceName) -> do
                    getMaterialInstanceData resourceData materialInstanceName
                meshData <- getMeshData resourceData meshName
                modelData <- Model.newModelData modelName meshData materialInstanceDatas
                HashTable.insert modelDataMap modelName modelData

    unloadModelDatas :: ResourceData -> RendererData -> IO ()
    unloadModelDatas resourceData rendererData = do
        clearHashTable (_modelDataMap resourceData) (\(k, v) -> Model.destroyModelData v)

    getModelData :: ResourceData -> Text.Text -> IO Model.ModelData
    getModelData resourceData resourceName = do
        getResourceData (_modelDataMap resourceData) resourceName defaultModelName

    -- Mesh Loader
    loadMeshDatas :: ResourceData -> RendererData -> IO ()
    loadMeshDatas resourceData rendererData = do
        registMeshData (_meshDataMap resourceData) "quad" GeometryBuffer.quadGeometryCreateInfo
        registMeshData (_meshDataMap resourceData) "cube" GeometryBuffer.cubeGeometryCreateInfo

        meshFiles <- walkDirectory meshFilePath [".obj"]
        forM_ meshFiles $ \meshFile -> do
            meshName <- getResourceNameFromFilepath (_meshDataMap resourceData) meshFilePath meshFile
            geometryCreateInfo <- loadMesh meshFile
            registMeshData (_meshDataMap resourceData) meshName geometryCreateInfo
        where
            registMeshData meshDataMap meshName geometryCreateInfo = do
                geometryBufferData <- createGeometryBuffer rendererData meshName geometryCreateInfo
                meshData <- newMeshData meshName [geometryBufferData]
                HashTable.insert (_meshDataMap resourceData) meshName meshData

    unloadMeshDatas :: ResourceData -> RendererData -> IO ()
    unloadMeshDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> (destroyGeometryData rendererData k v)) (_meshDataMap resourceData)
        where
            destroyGeometryData rendererData name meshData = do
                geometryDataCount <- getGeometryDataCount meshData
                forM_  [0..(geometryDataCount-1)] $ \index -> do
                    geometryData <- getGeometryData meshData index
                    destroyGeometryBuffer rendererData geometryData

    getMeshData :: ResourceData -> Text.Text -> IO MeshData
    getMeshData resourceData resourceName =
        getResourceData (_meshDataMap resourceData) resourceName defaultMeshName

    -- TextureLoader
    loadTextureDatas :: ResourceData -> RendererData -> IO ()
    loadTextureDatas resourceData rendererData = do
        textureFiles <- walkDirectory textureFilePath [".jpg", ".png"]
        forM_ textureFiles $ \textureFile -> do
            textureDataName <- getResourceNameFromFilepath (_textureDataMap resourceData) textureFilePath textureFile
            textureData <- createTexture rendererData textureDataName textureFile
            HashTable.insert (_textureDataMap resourceData) textureDataName textureData

    unloadTextureDatas :: ResourceData -> RendererData -> IO ()
    unloadTextureDatas resourceData rendererData =
        clearHashTable (_textureDataMap resourceData) (\(k, v) -> destroyTexture rendererData v)

    getTextureData :: ResourceData -> Text.Text -> IO TextureData
    getTextureData resourceData resourceName =
        getResourceData (_textureDataMap resourceData) resourceName defaultTextureName

    -- FrameBuffer
    loadFrameBufferDatas :: ResourceData -> RendererData -> IO ()
    loadFrameBufferDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> registFrameBufferData k) (_renderPassDataMap resourceData)
        where
            registFrameBufferData renderPassName = do
                Just renderPassData <- getRenderPassData resourceData renderPassName
                let frameBufferName = _renderPassFrameBufferName (renderPassData::RenderPassData)
                frameBufferDataCreateInfo <- FrameBufferCreateInfo.getFrameBufferDataCreateInfo rendererData frameBufferName
                frameBufferData <- createFrameBufferData (getDevice rendererData) (_renderPass renderPassData) frameBufferDataCreateInfo
                HashTable.insert (_frameBufferDataMap resourceData) frameBufferName frameBufferData

    unloadFrameBufferDatas :: ResourceData -> RendererData -> IO ()
    unloadFrameBufferDatas resourceData rendererData =
        clearHashTable (_frameBufferDataMap resourceData) (\(k, v) -> destroyFrameBufferData (getDevice rendererData) v)

    getFrameBufferData :: ResourceData -> Text.Text -> IO (Maybe FrameBufferData)
    getFrameBufferData resourceData resourceName =
       HashTable.lookup (_frameBufferDataMap resourceData) resourceName

    -- RenderPassLoader
    loadRenderPassDatas :: ResourceData -> RendererData -> IO ()
    loadRenderPassDatas resourceData rendererData = do
        registRenderPassData defaultRenderPassName
        registRenderPassData "composite_gbuffer"
        where
            registRenderPassData renderPassName = do
                renderPassDataCreateInfo <- RenderPassCreateInfo.getRenderPassDataCreateInfo rendererData renderPassName
                descriptorDatas <- forM (_pipelineDataCreateInfos renderPassDataCreateInfo) $ \pipelineDataCreateInfo -> do
                    getDescriptorData resourceData rendererData (_renderPassCreateInfoName renderPassDataCreateInfo) pipelineDataCreateInfo
                defaultRenderPassData <- createRenderPassData (getDevice rendererData) renderPassDataCreateInfo descriptorDatas
                HashTable.insert (_renderPassDataMap resourceData) (_renderPassDataName defaultRenderPassData) defaultRenderPassData

    unloadRenderPassDatas :: ResourceData -> RendererData -> IO ()
    unloadRenderPassDatas resourceData rendererData =
        clearHashTable (_renderPassDataMap resourceData) (\(k, v) -> destroyRenderPassData (getDevice rendererData) v)

    getRenderPassData :: ResourceData -> Text.Text -> IO (Maybe RenderPassData)
    getRenderPassData resourceData resourceName =
        HashTable.lookup (_renderPassDataMap resourceData) resourceName

    getDefaultRenderPassData :: ResourceData -> IO (Maybe RenderPassData)
    getDefaultRenderPassData resourceData =
        getRenderPassData resourceData defaultRenderPassName

    -- MaterialInstanceDatas
    loadMaterialInstanceDatas :: ResourceData -> RendererData -> IO ()
    loadMaterialInstanceDatas resourceData rendererData = do
        materialInstanceFiles <- walkDirectory materialInstanceFilePath [".matinst"]
        forM_ materialInstanceFiles $ \materialInstanceFile -> do
            materialInstanceName <- getResourceNameFromFilepath (_materialInstanceDataMap resourceData) materialInstanceFilePath materialInstanceFile
            contents <- ByteString.readFile materialInstanceFile
            registMaterialInstanceData rendererData (_materialInstanceDataMap resourceData) materialInstanceName contents
        where
            registMaterialInstanceData rendererData materialInstanceDataMap materialInstanceName contents = do
                let Just (Aeson.Object materialInstanceCreateInfoMap) = Aeson.decodeStrict contents
                    Just (Aeson.String renderPassDataName) = HashMap.lookup "render_pass_name" materialInstanceCreateInfoMap
                    Just (Aeson.String pipelineDataName) = HashMap.lookup "pipeline_name" materialInstanceCreateInfoMap
                    Just (Aeson.Object materialParameterMap) = HashMap.lookup "material_parameters" materialInstanceCreateInfoMap

                Just renderPassData <- getRenderPassData resourceData renderPassDataName
                pipelineData <- getPipelineData renderPassData pipelineDataName

                let descriptorDataCreateInfoList = Descriptor._descriptorDataCreateInfoList $ _descriptorData pipelineData
                descriptorResourceInfosList <- forM Constants.swapChainImageIndices $ \index -> do
                    descriptorResourceInfos <- forM descriptorDataCreateInfoList $ \descriptorDataCreateInfo -> do
                        let materialParameterName = Descriptor._descriptorName' descriptorDataCreateInfo
                            materialParameterType = Descriptor._descriptorType' descriptorDataCreateInfo
                            materialParameterResourceType = Descriptor._descriptorResourceType' descriptorDataCreateInfo
                            maybeMaterialParameter = HashMap.lookup materialParameterName materialParameterMap
                        case (materialParameterType, materialParameterResourceType) of
                            (VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, Descriptor.DescriptorResourceType_UniformBuffer) -> do
                                uniformBufferData <- getUniformBufferData rendererData materialParameterName
                                return $ Descriptor.DescriptorBufferInfo (_descriptorBufferInfos uniformBufferData !! index)
                            (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_Texture) -> do
                                textureData <- case maybeMaterialParameter of
                                    Just (Aeson.String value) -> getTextureData resourceData value
                                    otherwise -> getTextureData resourceData defaultTextureName
                                return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
                            (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_RenderTarget) -> do
                                textureData <- getRenderTarget rendererData materialParameterName
                                return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
                            otherwise -> return Descriptor.InvalidDescriptorInfo
                    return $ filter (/= Descriptor.InvalidDescriptorInfo) descriptorResourceInfos
                materialInstance <- createMaterialInstance (getDevice rendererData) materialInstanceName renderPassData pipelineData descriptorResourceInfosList
                HashTable.insert (_materialInstanceDataMap resourceData) materialInstanceName materialInstance

    unloadMaterialInstanceDatas :: ResourceData -> RendererData -> IO ()
    unloadMaterialInstanceDatas resourceData rendererData =
        clearHashTable (_materialInstanceDataMap resourceData) (\(k, v) -> destroyMaterialInstance (getDevice rendererData) v)

    reloadMaterialInstanceDatas :: ResourceData -> RendererData -> IO ()
    reloadMaterialInstanceDatas resourceData rendererData =
        flip HashTable.mapM_ (_modelDataMap resourceData) $ \(k, modelData) -> do
            materialInstances <- Model.getMaterialInstanceDataList modelData
            newMaterialInstances <- forM materialInstances $ \materialInstance ->
                getMaterialInstanceData resourceData (_materialInstanceName materialInstance)
            Model.setMaterialInstanceDataList modelData newMaterialInstances

    getMaterialInstanceData :: ResourceData -> Text.Text -> IO MaterialInstanceData
    getMaterialInstanceData resourceData resourceName =
        getResourceData (_materialInstanceDataMap resourceData) resourceName defaultMaterialInstanceName

    -- DescriptorDatas
    getDescriptorData :: ResourceData -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO Descriptor.DescriptorData
    getDescriptorData resourceData rendererData renderPassName pipelineDataCreateInfo = do
        let descriptorName = Text.append renderPassName (_pipelineDataCreateInfoName pipelineDataCreateInfo)
            descriptorDataCreateInfoList = _descriptorDataCreateInfoList (pipelineDataCreateInfo::PipelineDataCreateInfo)
            maxDescriptorPoolCount = Constants.maxDescriptorPoolAllocCount * Constants.descriptorSetCountAtOnce
        maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resourceData) descriptorName
        case maybeDescriptorData of
            (Just descriptorData) -> return descriptorData
            otherwise -> do
                descriptorData <- Descriptor.createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList maxDescriptorPoolCount
                HashTable.insert (_descriptorDataMap resourceData) descriptorName descriptorData
                return descriptorData

    unloadDescriptorDatas :: ResourceData -> RendererData -> IO ()
    unloadDescriptorDatas resourceData rendererData = do
        clearHashTable (_descriptorDataMap resourceData) (\(k, v) -> Descriptor.destroyDescriptorData (getDevice rendererData) v)
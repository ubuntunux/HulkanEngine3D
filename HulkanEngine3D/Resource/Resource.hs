{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeOperators          #-}

module HulkanEngine3D.Resource.Resource
    ( Resources (..)
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
import qualified Data.Map as Map

import Graphics.Vulkan.Core_1_0
--import Numeric.DataFrame

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
import HulkanEngine3D.Resource.ResourceData
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.UniformBuffer
import HulkanEngine3D.Utilities.Logger
--import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Utilities.System


gatherAllFiles :: Bool
gatherAllFiles = False

materialInstanceFilePath :: FilePath
materialInstanceFilePath = "Resource/MaterialInstances"

meshSourceFilePath :: FilePath
meshSourceFilePath = "Resource/Externals/Meshes"

meshFilePath :: FilePath
meshFilePath = "Resource/Meshes"

meshSourceExts :: [String]
meshSourceExts = [".obj"]

meshExt :: String
meshExt = ".mesh"

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
defaultMaterialInstanceName = "default"

defaultFrameBufferName :: Text.Text
defaultFrameBufferName = "render_default"

defaultRenderPassName :: Text.Text
defaultRenderPassName = "render_default"


type ResourceDataMap a = HashTable.BasicHashTable Text.Text a
type FrameBufferDataMap = ResourceDataMap FrameBufferData
type MaterialInstanceDataMap = ResourceDataMap MaterialInstanceData
type SceneManagerDataMap = ResourceDataMap SceneManagerData
type MeshDataMap = ResourceDataMap MeshData
type ModelDataMap = ResourceDataMap Model.ModelData
type TextureDataMap = ResourceDataMap TextureData
type RenderPassDataMap = ResourceDataMap RenderPassData
type DescriptorDataMap = ResourceDataMap Descriptor.DescriptorData
type MetaDataMap = ResourceDataMap MetaData

data Resources = Resources
    { _metaDataMap :: MetaDataMap
    , _meshDataMap :: MeshDataMap
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

getResourceNameFromFilePath :: FilePath -> FilePath -> Text.Text
getResourceNameFromFilePath resourcePath resourceFilePath = Text.pack $ drop (length resourcePath + 1) (dropExtension resourceFilePath)

getUniqueResourceName :: ResourceDataMap r -> FilePath -> FilePath -> IO Text.Text
getUniqueResourceName resourceDataMap resourcePath resourceFilePath = do
    let resourceName = getResourceNameFromFilePath resourcePath resourceFilePath
    generateUniqueName resourceDataMap resourceName

getResourceFileName :: FilePath -> Text.Text -> String -> FilePath
getResourceFileName resourceFilePath resourceName resourceExt = resourceFilePath ++ [pathSeparator] ++ Text.unpack resourceName ++ resourceExt

class ResourceInterface a where
    createResources :: IO a
    initializeResources :: a -> RendererData -> IO ()
    destroyResources :: a -> RendererData -> IO ()

    createResource :: a -> IO ()
    registResource :: a -> IO ()
    unregistResource :: a -> IO ()

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


instance ResourceInterface Resources where
    createResources :: IO Resources
    createResources = do
        metaDataMap <- HashTable.new
        frameBufferDataMap <- HashTable.new
        modelDataMap <- HashTable.new
        meshDataMap <- HashTable.new
        textureDataMap <- HashTable.new
        renderPassDataMap <- HashTable.new
        materialInstanceDataMap <- HashTable.new
        descriptorDataMap <- HashTable.new
        return Resources
            { _metaDataMap = metaDataMap
            , _frameBufferDataMap = frameBufferDataMap
            , _modelDataMap = modelDataMap
            , _meshDataMap = meshDataMap
            , _textureDataMap = textureDataMap
            , _renderPassDataMap = renderPassDataMap
            , _materialInstanceDataMap = materialInstanceDataMap
            , _descriptorDataMap = descriptorDataMap
            }

    initializeResources :: Resources -> RendererData -> IO ()
    initializeResources resources rendererData = do
        logInfo "initializeResources"
        loadTextureDatas resources rendererData
        loadRenderPassDatas resources rendererData
        loadFrameBufferDatas resources rendererData
        loadMaterialInstanceDatas resources rendererData
        loadMeshDatas resources rendererData
        loadModelDatas resources rendererData

    destroyResources :: Resources -> RendererData -> IO ()
    destroyResources resources rendererData = do
        logInfo "destroyResources"
        unloadModelDatas resources rendererData
        unloadMeshDatas resources rendererData
        unloadMaterialInstanceDatas resources rendererData
        unloadFrameBufferDatas resources rendererData
        unloadRenderPassDatas resources rendererData
        unloadTextureDatas resources rendererData
        unloadDescriptorDatas resources rendererData

    createResource :: Resources -> IO ()
    createResource resources = return ()

    registResource :: Resources -> IO ()
    registResource resources = return ()

    unregistResource :: Resources -> IO ()
    unregistResource resources = return ()

    -- GraphicsDatas
    loadGraphicsDatas :: Resources -> RendererData -> IO ()
    loadGraphicsDatas resources rendererData = do
        logInfo "Resources::loadGraphicsDatas"
        loadRenderPassDatas resources rendererData
        loadFrameBufferDatas resources rendererData
        loadMaterialInstanceDatas resources rendererData
        reloadMaterialInstanceDatas resources rendererData

    unloadGraphicsDatas :: Resources -> RendererData -> IO ()
    unloadGraphicsDatas resources rendererData = do
        logInfo "Resources::unloadGraphicsDatas"
        unloadMaterialInstanceDatas resources rendererData
        unloadFrameBufferDatas resources rendererData
        unloadRenderPassDatas resources rendererData
        unloadDescriptorDatas resources rendererData

    -- SceneManagerData
    loadSceneManagerDatas :: Resources -> RendererData -> IO ()
    loadSceneManagerDatas resources rendererData = return ()

    unloadSceneManagerDatas :: Resources -> RendererData -> IO ()
    unloadSceneManagerDatas resources rendererData = return ()

    -- Model Loader
    loadModelDatas :: Resources -> RendererData -> IO ()
    loadModelDatas resources rendererData = do
        modelFiles <- walkDirectory modelFilePath [".model"]
        forM_ modelFiles $ \modelFile -> do
            modelName <- getUniqueResourceName (_modelDataMap resources) modelFilePath modelFile
            contents <- ByteString.readFile modelFile
            registModelData (_modelDataMap resources) modelName contents
        where
            registModelData modelDataMap modelName contents = do
                let Just (Aeson.Object modelCreateInfoMap) = Aeson.decodeStrict contents
                    Just (Aeson.Array materialInstanceNames) = HashMap.lookup "material_instances" modelCreateInfoMap
                    materialInstanceCount = Vector.length materialInstanceNames
                    Just (Aeson.String meshName) = HashMap.lookup "mesh" modelCreateInfoMap
                meshData <- getMeshData resources meshName
                geometryDataCount <- getGeometryDataCount meshData
                let materialInstanceNameList = (Vector.take geometryDataCount materialInstanceNames) Vector.++ (Vector.replicate (max 0 (geometryDataCount - materialInstanceCount)) (Aeson.String defaultMaterialInstanceName))
                materialInstanceDatas <- forM (Vector.toList materialInstanceNameList) $ \(Aeson.String materialInstanceName) ->
                    getMaterialInstanceData resources materialInstanceName
                modelData <- Model.newModelData modelName meshData materialInstanceDatas
                HashTable.insert modelDataMap modelName modelData

    unloadModelDatas :: Resources -> RendererData -> IO ()
    unloadModelDatas resources rendererData = do
        clearHashTable (_modelDataMap resources) (\(k, v) -> Model.destroyModelData v)

    getModelData :: Resources -> Text.Text -> IO Model.ModelData
    getModelData resources resourceName = do
        getResourceData (_modelDataMap resources) resourceName defaultModelName

    -- Mesh Loader
    loadMeshDatas :: Resources -> RendererData -> IO ()
    loadMeshDatas resources rendererData = do
        registMeshData (_meshDataMap resources) "quad" GeometryBuffer.quadGeometryCreateInfos
        registMeshData (_meshDataMap resources) "cube" GeometryBuffer.cubeGeometryCreateInfos

        meshFiles <- walkDirectory meshFilePath [meshExt]
        let meshFileMap = Map.fromList $ map (\meshFile -> (getResourceNameFromFilePath meshFilePath meshFile, meshFile)) meshFiles
        meshSourceFiles <- walkDirectory meshSourceFilePath meshSourceExts
        forM_ meshSourceFiles $ \meshSourceFile -> do
            meshName <- getUniqueResourceName (_meshDataMap resources) meshSourceFilePath meshSourceFile
            geometryCreateInfos <- loadMesh meshSourceFile
            Aeson.encodeFile (getResourceFileName meshFilePath meshName meshExt) geometryCreateInfos
--            case Map.lookup meshName meshFileMap of
--                Just _ -> return ()
--                otherwise -> Aeson.encodeFile (getResourceFileName meshFilePath meshName meshExt) geometryCreateInfos
            registMeshData (_meshDataMap resources) meshName geometryCreateInfos
        where
            registMeshData meshDataMap meshName geometryCreateInfos = do
                geometryBufferDatas <- forM (zip ([0..]::[Int]) geometryCreateInfos) $ \(index, geometryCreateInfo) -> do
--                    let xxx = map (\x -> unScalar . fromIntegral $ x) $ dataFrameToList (GeometryBuffer._geometryCreateInfoIndices geometryCreateInfo)::[Word32]
--                    logInfo $ show (dataFrameToList (GeometryBuffer._geometryCreateInfoIndices geometryCreateInfo))
                    createGeometryBuffer rendererData (Text.append meshName (Text.pack $ show index)) geometryCreateInfo
                meshData <- newMeshData meshName geometryBufferDatas

                HashTable.insert (_meshDataMap resources) meshName meshData

    unloadMeshDatas :: Resources -> RendererData -> IO ()
    unloadMeshDatas resources rendererData = do
        HashTable.mapM_ (\(k, v) -> (destroyGeometryData rendererData k v)) (_meshDataMap resources)
        where
            destroyGeometryData rendererData name meshData = do
                geometryDataCount <- getGeometryDataCount meshData
                forM_  [0..(geometryDataCount - 1)] $ \index -> do
                    geometryData <- getGeometryData meshData index
                    destroyGeometryBuffer rendererData geometryData

    getMeshData :: Resources -> Text.Text -> IO MeshData
    getMeshData resources resourceName =
        getResourceData (_meshDataMap resources) resourceName defaultMeshName

    -- TextureLoader
    loadTextureDatas :: Resources -> RendererData -> IO ()
    loadTextureDatas resources rendererData = do
        textureFiles <- walkDirectory textureFilePath [".jpg", ".png"]
        forM_ textureFiles $ \textureFile -> do
            textureDataName <- getUniqueResourceName (_textureDataMap resources) textureFilePath textureFile
            textureData <- createTexture rendererData textureDataName textureFile
            HashTable.insert (_textureDataMap resources) textureDataName textureData

    unloadTextureDatas :: Resources -> RendererData -> IO ()
    unloadTextureDatas resources rendererData =
        clearHashTable (_textureDataMap resources) (\(k, v) -> destroyTexture rendererData v)

    getTextureData :: Resources -> Text.Text -> IO TextureData
    getTextureData resources resourceName =
        getResourceData (_textureDataMap resources) resourceName defaultTextureName

    -- FrameBuffer
    loadFrameBufferDatas :: Resources -> RendererData -> IO ()
    loadFrameBufferDatas resources rendererData = do
        HashTable.mapM_ (\(k, v) -> registFrameBufferData k) (_renderPassDataMap resources)
        where
            registFrameBufferData renderPassName = do
                Just renderPassData <- getRenderPassData resources renderPassName
                let frameBufferName = _renderPassFrameBufferName (renderPassData::RenderPassData)
                frameBufferDataCreateInfo <- FrameBufferCreateInfo.getFrameBufferDataCreateInfo rendererData frameBufferName
                frameBufferData <- createFrameBufferData (getDevice rendererData) (_renderPass renderPassData) frameBufferDataCreateInfo
                HashTable.insert (_frameBufferDataMap resources) frameBufferName frameBufferData

    unloadFrameBufferDatas :: Resources -> RendererData -> IO ()
    unloadFrameBufferDatas resources rendererData =
        clearHashTable (_frameBufferDataMap resources) (\(k, v) -> destroyFrameBufferData (getDevice rendererData) v)

    getFrameBufferData :: Resources -> Text.Text -> IO (Maybe FrameBufferData)
    getFrameBufferData resources resourceName =
       HashTable.lookup (_frameBufferDataMap resources) resourceName

    -- RenderPassLoader
    loadRenderPassDatas :: Resources -> RendererData -> IO ()
    loadRenderPassDatas resources rendererData = do
        registRenderPassData defaultRenderPassName
        registRenderPassData "composite_gbuffer"
        where
            registRenderPassData renderPassName = do
                renderPassDataCreateInfo <- RenderPassCreateInfo.getRenderPassDataCreateInfo rendererData renderPassName
                descriptorDatas <- forM (_pipelineDataCreateInfos renderPassDataCreateInfo) $ \pipelineDataCreateInfo -> do
                    getDescriptorData resources rendererData (_renderPassCreateInfoName renderPassDataCreateInfo) pipelineDataCreateInfo
                defaultRenderPassData <- createRenderPassData (getDevice rendererData) renderPassDataCreateInfo descriptorDatas
                HashTable.insert (_renderPassDataMap resources) (_renderPassDataName defaultRenderPassData) defaultRenderPassData

    unloadRenderPassDatas :: Resources -> RendererData -> IO ()
    unloadRenderPassDatas resources rendererData =
        clearHashTable (_renderPassDataMap resources) (\(k, v) -> destroyRenderPassData (getDevice rendererData) v)

    getRenderPassData :: Resources -> Text.Text -> IO (Maybe RenderPassData)
    getRenderPassData resources resourceName =
        HashTable.lookup (_renderPassDataMap resources) resourceName

    getDefaultRenderPassData :: Resources -> IO (Maybe RenderPassData)
    getDefaultRenderPassData resources =
        getRenderPassData resources defaultRenderPassName

    -- MaterialInstanceDatas
    loadMaterialInstanceDatas :: Resources -> RendererData -> IO ()
    loadMaterialInstanceDatas resources rendererData = do
        materialInstanceFiles <- walkDirectory materialInstanceFilePath [".matinst"]
        forM_ materialInstanceFiles $ \materialInstanceFile -> do
            materialInstanceName <- getUniqueResourceName (_materialInstanceDataMap resources) materialInstanceFilePath materialInstanceFile
            contents <- ByteString.readFile materialInstanceFile
            registMaterialInstanceData rendererData (_materialInstanceDataMap resources) materialInstanceName contents
        where
            registMaterialInstanceData rendererData materialInstanceDataMap materialInstanceName contents = do
                let Just (Aeson.Object materialInstanceCreateInfoMap) = Aeson.decodeStrict contents
                    Just (Aeson.String renderPassDataName) = HashMap.lookup "render_pass_name" materialInstanceCreateInfoMap
                    Just (Aeson.String pipelineDataName) = HashMap.lookup "pipeline_name" materialInstanceCreateInfoMap
                    Just (Aeson.Object materialParameterMap) = HashMap.lookup "material_parameters" materialInstanceCreateInfoMap

                Just renderPassData <- getRenderPassData resources renderPassDataName
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
                                    Just (Aeson.String value) -> getTextureData resources value
                                    otherwise -> getTextureData resources defaultTextureName
                                return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
                            (VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.DescriptorResourceType_RenderTarget) -> do
                                textureData <- getRenderTarget rendererData materialParameterName
                                return $ Descriptor.DescriptorImageInfo (_descriptorImageInfo textureData)
                            otherwise -> return Descriptor.InvalidDescriptorInfo
                    return $ filter (/= Descriptor.InvalidDescriptorInfo) descriptorResourceInfos
                materialInstance <- createMaterialInstance (getDevice rendererData) materialInstanceName renderPassData pipelineData descriptorResourceInfosList
                HashTable.insert (_materialInstanceDataMap resources) materialInstanceName materialInstance

    unloadMaterialInstanceDatas :: Resources -> RendererData -> IO ()
    unloadMaterialInstanceDatas resources rendererData =
        clearHashTable (_materialInstanceDataMap resources) (\(k, v) -> destroyMaterialInstance (getDevice rendererData) v)

    reloadMaterialInstanceDatas :: Resources -> RendererData -> IO ()
    reloadMaterialInstanceDatas resources rendererData =
        flip HashTable.mapM_ (_modelDataMap resources) $ \(k, modelData) -> do
            materialInstances <- Model.getMaterialInstanceDataList modelData
            newMaterialInstances <- forM materialInstances $ \materialInstance ->
                getMaterialInstanceData resources (_materialInstanceName materialInstance)
            Model.setMaterialInstanceDataList modelData newMaterialInstances

    getMaterialInstanceData :: Resources -> Text.Text -> IO MaterialInstanceData
    getMaterialInstanceData resources resourceName =
        getResourceData (_materialInstanceDataMap resources) resourceName defaultMaterialInstanceName

    -- DescriptorDatas
    getDescriptorData :: Resources -> RendererData -> Text.Text -> PipelineDataCreateInfo -> IO Descriptor.DescriptorData
    getDescriptorData resources rendererData renderPassName pipelineDataCreateInfo = do
        let descriptorName = Text.append renderPassName (_pipelineDataCreateInfoName pipelineDataCreateInfo)
            descriptorDataCreateInfoList = _descriptorDataCreateInfoList (pipelineDataCreateInfo::PipelineDataCreateInfo)
            maxDescriptorPoolCount = Constants.maxDescriptorPoolAllocCount * Constants.descriptorSetCountAtOnce
        maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resources) descriptorName
        case maybeDescriptorData of
            (Just descriptorData) -> return descriptorData
            otherwise -> do
                descriptorData <- Descriptor.createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList maxDescriptorPoolCount
                HashTable.insert (_descriptorDataMap resources) descriptorName descriptorData
                return descriptorData

    unloadDescriptorDatas :: Resources -> RendererData -> IO ()
    unloadDescriptorDatas resources rendererData = do
        clearHashTable (_descriptorDataMap resources) (\(k, v) -> Descriptor.destroyDescriptorData (getDevice rendererData) v)
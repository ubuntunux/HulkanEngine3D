{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.Resource
    ( ResourceData (..)
    , ResourceInterface (..)
    ) where

import Data.IORef
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.Vector.Mutable as MVector

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Mesh
import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Resource.ObjLoader
import HulkanEngine3D.Resource.RenderPassLoader
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.RenderPass

type MaterialInstanceDataMap = HashTable.BasicHashTable Text.Text MaterialInstanceData
type MeshDataMap = HashTable.BasicHashTable Text.Text MeshData
type TextureDataMap = HashTable.BasicHashTable Text.Text TextureData
type RenderPassDataMap = HashTable.BasicHashTable Text.Text RenderPassData
type DescriptorDataMap = HashTable.BasicHashTable Text.Text DescriptorData

data ResourceData = ResourceData
    { _meshDataMap :: MeshDataMap
    , _textureDataMap :: TextureDataMap
    , _renderPassDataMap :: RenderPassDataMap
    , _materialInstanceDataMap :: MaterialInstanceDataMap
    , _descriptorDataMap :: DescriptorDataMap
    } deriving (Show)


class ResourceInterface a where
    createNewResourceData :: IO a
    initializeResourceData :: a -> RendererData -> IO ()
    destroyResourceData :: a -> RendererData -> IO ()

    recreateGraphicsDatas :: a -> RendererData -> IO ()
    destroyGraphicsDatas :: a -> RendererData -> IO ()

    loadMeshDatas :: a -> RendererData -> IO ()
    unloadMeshDatas :: a -> RendererData -> IO ()
    getMeshData :: a -> Text.Text -> IO (Maybe MeshData)

    loadTextureDatas :: a -> RendererData -> IO ()
    unloadTextureDatas :: a -> RendererData -> IO ()
    getTextureData :: a -> Text.Text -> IO (Maybe TextureData)

    loadRenderPassDatas :: a -> RendererData -> IO ()
    unloadRenderPassDatas :: a -> RendererData -> IO ()
    getRenderPassData :: a -> Text.Text -> IO (Maybe RenderPassData)
    getDefaultRenderPassData :: a -> IO (Maybe RenderPassData)

    loadMaterialInstanceDatas :: a -> RendererData -> IO ()
    unloadMaterialInstanceDatas :: a -> RendererData -> IO ()
    getMaterialInstanceData :: a -> Text.Text -> IO (Maybe MaterialInstanceData)
    getDefaultMaterialInstanceData :: a -> IO (Maybe MaterialInstanceData)

    getDescriptorData :: a -> RendererData -> RenderPassDataCreateInfo -> IO DescriptorData
    unloadDescriptorDatas :: a -> RendererData -> IO ()

instance ResourceInterface ResourceData where
    createNewResourceData :: IO ResourceData
    createNewResourceData = do
        meshDataMap <- HashTable.new
        textureDataMap <- HashTable.new
        renderPassDataMap <- HashTable.new
        materialInstanceDataMap <- HashTable.new
        descriptorDataMap <- HashTable.new
        return ResourceData
            { _meshDataMap = meshDataMap
            , _textureDataMap = textureDataMap
            , _renderPassDataMap = renderPassDataMap
            , _materialInstanceDataMap = materialInstanceDataMap
            , _descriptorDataMap = descriptorDataMap
            }

    initializeResourceData :: ResourceData -> RendererData -> IO ()
    initializeResourceData resourceData rendererData = do
        loadMeshDatas resourceData rendererData
        loadTextureDatas resourceData rendererData
        loadRenderPassDatas resourceData rendererData
        loadMaterialInstanceDatas resourceData rendererData

    destroyResourceData :: ResourceData -> RendererData -> IO ()
    destroyResourceData resourceData rendererData = do
        unloadMeshDatas resourceData rendererData
        unloadTextureDatas resourceData rendererData
        unloadDescriptorDatas resourceData rendererData
        unloadRenderPassDatas resourceData rendererData
        unloadMaterialInstanceDatas resourceData rendererData

    -- GraphicsDatas
    recreateGraphicsDatas :: ResourceData -> RendererData -> IO ()
    recreateGraphicsDatas resourceData rendererData =
        loadRenderPassDatas resourceData rendererData

    destroyGraphicsDatas :: ResourceData -> RendererData -> IO ()
    destroyGraphicsDatas resourceData rendererData =
        unloadRenderPassDatas resourceData rendererData

    -- Mesh Loader
    loadMeshDatas :: ResourceData -> RendererData -> IO ()
    loadMeshDatas resourceData rendererData = do
        let name = "suzan"::Text.Text
        (vertices, indices) <- loadModel "Resource/Externals/Meshes/suzan.obj"
        geometryBufferData <- createGeometryBuffer rendererData name vertices indices
        meshData <- newMeshData name [geometryBufferData]
        HashTable.insert (_meshDataMap resourceData) name meshData

    unloadMeshDatas :: ResourceData -> RendererData -> IO ()
    unloadMeshDatas resourceData rendererData = do
        HashTable.mapM_ (\(k, v) -> (action rendererData k v)) (_meshDataMap resourceData)
        where
            action rendererData name meshData = do
                let count = MVector.length (_geometryBufferDatas meshData)
                    loop x
                        | x < count = do
                            geometryBufferData <- MVector.unsafeRead (_geometryBufferDatas meshData) x
                            destroyGeometryBuffer rendererData geometryBufferData
                            loop (x+1)
                        | otherwise = return ()
                loop 0

    getMeshData :: ResourceData -> Text.Text -> IO (Maybe MeshData)
    getMeshData resourceData resourceName =
        HashTable.lookup (_meshDataMap resourceData) resourceName

    -- TextureLoader
    loadTextureDatas :: ResourceData -> RendererData -> IO ()
    loadTextureDatas resourceData rendererData = do
        textureData <- createTexture rendererData "Resource/Externals/Textures/texture.jpg"
        HashTable.insert (_textureDataMap resourceData) "texture" textureData

    unloadTextureDatas :: ResourceData -> RendererData -> IO ()
    unloadTextureDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyTexture rendererData v) (_textureDataMap resourceData)

    getTextureData :: ResourceData -> Text.Text -> IO (Maybe TextureData)
    getTextureData resourceData resourceName =
        HashTable.lookup (_textureDataMap resourceData) resourceName

    -- RenderPassLoader
    loadRenderPassDatas :: ResourceData -> RendererData -> IO ()
    loadRenderPassDatas resourceData rendererData = do
        renderTargets <- readIORef (_renderTargets rendererData)
        swapChainData <- readIORef (_swapChainDataRef rendererData)

        defaultRenderPassDataCreateInfo <- getRenderPassDataCreateInfo rendererData "defaultRenderPass"
        descriptorData <- getDescriptorData resourceData rendererData defaultRenderPassDataCreateInfo
        defaultRenderPassData <- createRenderPassData (getDevice rendererData) defaultRenderPassDataCreateInfo descriptorData
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
    loadMaterialInstanceDatas resourceData rendererData = return ()

    unloadMaterialInstanceDatas :: ResourceData -> RendererData -> IO ()
    unloadMaterialInstanceDatas resourceData rendererData = return ()

    getMaterialInstanceData :: ResourceData -> Text.Text -> IO (Maybe MaterialInstanceData)
    getMaterialInstanceData resourceData resourceName = return undefined

    getDefaultMaterialInstanceData :: ResourceData -> IO (Maybe MaterialInstanceData)
    getDefaultMaterialInstanceData resourceData = return undefined

    -- DescriptorDatas
    getDescriptorData :: ResourceData -> RendererData -> RenderPassDataCreateInfo -> IO DescriptorData
    getDescriptorData resourceData rendererData renderPassDataCreateInfo = do
        let renderPassName = _renderPassCreateInfoName renderPassDataCreateInfo
            pipelineDataCreateInfo = _pipelineDataCreateInfo renderPassDataCreateInfo
            pipelineDataName = _pipelineDataCreateInfoName pipelineDataCreateInfo
            descriptorDataCreateInfoList = _descriptorDataCreateInfoList pipelineDataCreateInfo
            descriptorCount = Constants.swapChainImageCount
            resourceName = Text.append renderPassName (Text.append "_" pipelineDataName)
        maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resourceData) resourceName
        case maybeDescriptorData of
            (Just descriptorData) -> return descriptorData
            otherwise -> do
                descriptorData <- createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList descriptorCount
                HashTable.insert (_descriptorDataMap resourceData) resourceName descriptorData
                return descriptorData

    unloadDescriptorDatas :: ResourceData -> RendererData -> IO ()
    unloadDescriptorDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyDescriptorData (getDevice rendererData) v) (_descriptorDataMap resourceData)
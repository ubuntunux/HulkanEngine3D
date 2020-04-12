{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.Resource
    ( ResourceData (..)
    , ResourceInterface (..)
    ) where

import Control.Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.Vector.Mutable as MVector

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Mesh
import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Resource.ObjLoader
import HulkanEngine3D.Resource.RenderPassDatas
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Utilities.Logger


type FrameBufferDataMap = HashTable.BasicHashTable Text.Text FrameBufferData
type MaterialInstanceDataMap = HashTable.BasicHashTable Text.Text MaterialInstanceData
type MeshDataMap = HashTable.BasicHashTable Text.Text MeshData
type TextureDataMap = HashTable.BasicHashTable Text.Text TextureData
type RenderPassDataMap = HashTable.BasicHashTable Text.Text RenderPassData
type DescriptorDataMap = HashTable.BasicHashTable Text.Text DescriptorData

data ResourceData = ResourceData
    { _meshDataMap :: MeshDataMap
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

    getDescriptorData :: a -> RendererData -> PipelineDataCreateInfo -> IO DescriptorData
    unloadDescriptorDatas :: a -> RendererData -> IO ()

instance ResourceInterface ResourceData where
    createNewResourceData :: IO ResourceData
    createNewResourceData = do
        frameBufferDataMap <- HashTable.new
        meshDataMap <- HashTable.new
        textureDataMap <- HashTable.new
        renderPassDataMap <- HashTable.new
        materialInstanceDataMap <- HashTable.new
        descriptorDataMap <- HashTable.new
        return ResourceData
            { _frameBufferDataMap = frameBufferDataMap
            , _meshDataMap = meshDataMap
            , _textureDataMap = textureDataMap
            , _renderPassDataMap = renderPassDataMap
            , _materialInstanceDataMap = materialInstanceDataMap
            , _descriptorDataMap = descriptorDataMap
            }

    initializeResourceData :: ResourceData -> RendererData -> IO ()
    initializeResourceData resourceData rendererData = do
        logInfo "initializeResourceData"
        loadMeshDatas resourceData rendererData
        loadTextureDatas resourceData rendererData
        loadRenderPassDatas resourceData rendererData
        loadFrameBufferDatas resourceData rendererData
        loadMaterialInstanceDatas resourceData rendererData

    destroyResourceData :: ResourceData -> RendererData -> IO ()
    destroyResourceData resourceData rendererData = do
        logInfo "destroyResourceData"
        unloadMeshDatas resourceData rendererData
        unloadTextureDatas resourceData rendererData
        unloadDescriptorDatas resourceData rendererData
        unloadRenderPassDatas resourceData rendererData
        unloadFrameBufferDatas resourceData rendererData
        unloadMaterialInstanceDatas resourceData rendererData

    -- GraphicsDatas
    loadGraphicsDatas :: ResourceData -> RendererData -> IO ()
    loadGraphicsDatas resourceData rendererData = do
        loadFrameBufferDatas resourceData rendererData

    unloadGraphicsDatas :: ResourceData -> RendererData -> IO ()
    unloadGraphicsDatas resourceData rendererData = do
        unloadFrameBufferDatas resourceData rendererData

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
            getDescriptorData resourceData rendererData pipelineDataCreateInfo
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
    loadMaterialInstanceDatas resourceData rendererData = return ()

    unloadMaterialInstanceDatas :: ResourceData -> RendererData -> IO ()
    unloadMaterialInstanceDatas resourceData rendererData = return ()

    getMaterialInstanceData :: ResourceData -> Text.Text -> IO (Maybe MaterialInstanceData)
    getMaterialInstanceData resourceData resourceName = return undefined

    getDefaultMaterialInstanceData :: ResourceData -> IO (Maybe MaterialInstanceData)
    getDefaultMaterialInstanceData resourceData = return undefined

    -- DescriptorDatas
    getDescriptorData :: ResourceData -> RendererData -> PipelineDataCreateInfo -> IO DescriptorData
    getDescriptorData resourceData rendererData pipelineDataCreateInfo = do
        let pipelineDataName = _pipelineDataCreateInfoName pipelineDataCreateInfo
            descriptorDataCreateInfoList = _descriptorDataCreateInfoList pipelineDataCreateInfo
            descriptorCount = Constants.swapChainImageCount
        maybeDescriptorData <- HashTable.lookup (_descriptorDataMap resourceData) pipelineDataName
        case maybeDescriptorData of
            (Just descriptorData) -> return descriptorData
            otherwise -> do
                descriptorData <- createDescriptorData (getDevice rendererData) descriptorDataCreateInfoList descriptorCount
                HashTable.insert (_descriptorDataMap resourceData) pipelineDataName descriptorData
                return descriptorData

    unloadDescriptorDatas :: ResourceData -> RendererData -> IO ()
    unloadDescriptorDatas resourceData rendererData =
        HashTable.mapM_ (\(k, v) -> destroyDescriptorData (getDevice rendererData) v) (_descriptorDataMap resourceData)
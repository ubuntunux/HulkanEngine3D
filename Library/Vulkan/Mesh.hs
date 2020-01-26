{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Library.Vulkan.Mesh
  ( Vertex (..)
  , Tri (..)
  , GeometryBuffer(..)
  , createGeometryBuffer
  , destroyGeometryBuffers
  , createVertexBuffer
  , createIndexBuffer
  ) where

import GHC.Generics (Generic)
import Data.Bits
import Foreign.Ptr (castPtr)
import Foreign.Storable
import qualified Data.DList as DList
import Codec.Wavefront
import Numeric.DataFrame
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import Library.Utils
import Library.Vulkan
import Library.Vulkan.Buffer



-- | Preparing Vertex data to make an interleaved array.
data Vertex = Vertex
  { pos      :: Vec3f
  , color    :: Vec3f
  , texCoord :: Vec2f
  } deriving (Eq, Ord, Show, Generic)

data Tri = Tri {-# UNPACK #-}!FaceIndex
               {-# UNPACK #-}!FaceIndex
               {-# UNPACK #-}!FaceIndex

instance PrimBytes Vertex



data GeometryBuffer = GeometryBuffer
    { _bufferName :: String
    , _vertexBufferMemory :: VkDeviceMemory
    , _vertexBuffer :: VkBuffer
    , _indexBufferMemory :: VkDeviceMemory
    , _indexBuffer :: VkBuffer
    }  deriving (Eq, Show, Generic)


createGeometryBuffer:: String
                    -> RenderData
                    -> DataFrame Vertex '[XN 3]
                    -> DataFrame Word32 '[XN 3]
                    -> IO GeometryBuffer
createGeometryBuffer bufferName renderData vertices indices = do
    putStrLn $ "createGeometryBuffer : "  ++ bufferName
    (vertexBufferMemory, vertexBuffer) <- createVertexBuffer renderData vertices
    (indexBufferMemory, indexBuffer) <- createIndexBuffer renderData indices
    return GeometryBuffer { _bufferName = bufferName
                          , _vertexBufferMemory = vertexBufferMemory
                          , _vertexBuffer = vertexBuffer
                          , _indexBufferMemory = indexBufferMemory
                          , _indexBuffer = indexBuffer }

destroyGeometryBuffers :: VkDevice -> DList.DList GeometryBuffer -> IO ()
destroyGeometryBuffers device geometryBufferList = do
    let geometryBuffer = (DList.toList geometryBufferList) !! 0
    destroyBuffer device (_vertexBuffer geometryBuffer) (_vertexBufferMemory geometryBuffer)
    destroyBuffer device (_indexBuffer geometryBuffer) (_indexBufferMemory geometryBuffer)
    return ()


createVertexBuffer :: RenderData
                   -> DataFrame Vertex '[XN 3]
                   -> IO (VkDeviceMemory, VkBuffer)
createVertexBuffer renderData (XFrame vertices) = do
    let device = _device renderData
        physicalDevice = _physicalDevice renderData
        graphicsQueue = _graphicsQueue $ _queueFamilyDatas renderData
        commandPool = _commandPool renderData
        bufferSize = bSizeOf vertices
        bufferUsageFlags = (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
        memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- create vertex buffer
    (vertexBufferMemory, vertexBuffer) <- createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags

    let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
        stagingBufferMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

    -- create temporary staging buffer
    (stagingBufferMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingBufferMemoryPropertyFlags

    -- copy data
    stagingDataPtr <- allocaPeek $ vkMapMemory device stagingBufferMemory 0 bufferSize VK_ZERO_FLAGS
    poke (castPtr stagingDataPtr) vertices
    vkUnmapMemory device stagingBufferMemory
    copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer bufferSize

    -- destroy temporary staging buffer
    destroyBuffer device stagingBuffer stagingBufferMemory

    return (vertexBufferMemory, vertexBuffer)


createIndexBuffer :: RenderData
                  -> DataFrame Word32 '[XN 3]
                  -> IO (VkDeviceMemory, VkBuffer)
createIndexBuffer renderData (XFrame indices) = do
    let device = _device renderData
        physicalDevice = _physicalDevice renderData
        graphicsQueue = _graphicsQueue $ _queueFamilyDatas renderData
        commandPool = _commandPool renderData
        bufferSize = bSizeOf indices
        bufferUsageFlags = (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
        memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- create index buffer
    (indexBufferMemory, indexBuffer) <- createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags

    -- create temporary staging buffer
    let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
        stagingBufferMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (stagingBufferMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingBufferMemoryPropertyFlags

    -- copy data
    stagingDataPtr <- allocaPeek $ vkMapMemory device stagingBufferMemory 0 bufferSize VK_ZERO_FLAGS
    poke (castPtr stagingDataPtr) indices
    vkUnmapMemory device stagingBufferMemory
    copyBuffer device commandPool graphicsQueue stagingBuffer indexBuffer bufferSize

    -- destroy temporary staging buffer
    destroyBuffer device stagingBuffer stagingBufferMemory

    return (indexBufferMemory, indexBuffer)

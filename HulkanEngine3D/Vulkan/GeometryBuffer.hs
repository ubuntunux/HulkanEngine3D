{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE NegativeLiterals       #-}

module HulkanEngine3D.Vulkan.GeometryBuffer where

import GHC.Generics (Generic)
import qualified Control.Monad.ST as ST
import Data.Bits
import qualified Data.Text as Text
import Foreign.Ptr (castPtr)
import Foreign.Storable
import qualified Codec.Wavefront as Wavefront
import Data.Maybe

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import qualified Numeric.DataFrame.ST as ST
import Numeric.DataFrame
import Numeric.Dimensions

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Vulkan.Buffer


-- | Preparing Vertex data to make an interleaved array.
data Vertex = Vertex
    { pos :: Vec3f
    , normal :: Vec3f
    , color :: Vec4f
    , texCoord :: Vec2f
    } deriving (Eq, Ord, Show, Generic)

instance PrimBytes Vertex

data Triangle = Triangle {-# UNPACK #-}!Wavefront.FaceIndex
                         {-# UNPACK #-}!Wavefront.FaceIndex
                         {-# UNPACK #-}!Wavefront.FaceIndex

type DataFrameAtLeastThree a = DataFrame a '[XN 3]

data GeometryData = GeometryData
    { _geometryName :: Text.Text
    , _vertexBufferMemory :: VkDeviceMemory
    , _vertexBuffer :: VkBuffer
    , _indexBufferMemory :: VkDeviceMemory
    , _indexBuffer :: VkBuffer
    , _vertexIndexCount :: Word32
    }  deriving (Eq, Show, Generic)


-- | Check if the frame has enough elements.
atLeastThree :: (All KnownXNatType ns, BoundedDims ns)
             => DataFrame t (n ': ns)
             -> DataFrame t (XN 3 ': ns)
atLeastThree = fromMaybe (error "Lib.Vulkan.Vertex.atLeastThree: not enough points")
             . constrainDF

-- | Interleaved array of vertices containing at least 3 entries.
--
--   Obviously, in real world vertices come from a separate file and not known at compile time.
--   The shader pipeline requires at least 3 unique vertices (for a triangle)
--   to render something on a screen. Setting `XN 3` here is just a handy way
--   to statically ensure the program satisfies this requirement.
--   This way, not-enough-vertices error occures at the moment of DataFrame initialization
--   instead of silently failing to render something onto a screen.
--
--   Note: in this program, `n >= 3` requirement is also forced in `Lib/Vulkan/VertexBuffer.hs`,
--         where it is not strictly necessary but allows to avoid specifying DataFrame constraints
--         in function signatures (such as, e.g. `KnownDim n`).
quadVertices :: DataFrameAtLeastThree Vertex
quadVertices = XFrame $ fromFlatList (D4 :* U) (Vertex 0 0 0 0)
    [ Vertex (vec3 -1.0 -1.0  0.0) (vec3 0 1 0) (vec4 1 1 1 1) (vec2 0 0)
    , Vertex (vec3  1.0 -1.0  0.0) (vec3 0 1 0) (vec4 1 1 1 1) (vec2 1 0)
    , Vertex (vec3  1.0  1.0  0.0) (vec3 0 1 0) (vec4 1 1 1 1) (vec2 1 1)
    , Vertex (vec3 -1.0  1.0  0.0) (vec3 0 1 0) (vec4 1 1 1 1) (vec2 0 1)
    ]

quadIndices :: DataFrameAtLeastThree Word32
quadIndices = atLeastThree . fromList $ [0, 3, 2, 2, 1, 0]

cubeVertices :: DataFrameAtLeastThree Vertex
cubeVertices =
    let positions = [vec3 x y z | position@(x, y, z) <- [
            (-1, 1, 1), (-1, -1, 1), (1, -1, 1), (1, 1, 1),
            (1, 1, 1), (1, -1, 1), (1, -1, -1), (1, 1, -1),
            (1, 1, -1), (1, -1, -1), (-1, -1, -1), (-1, 1, -1),
            (-1, 1, -1), (-1, -1, -1), (-1, -1, 1), (-1, 1, 1),
            (-1, 1, -1), (-1, 1, 1), (1, 1, 1), (1, 1, -1),
            (-1, -1, 1), (-1, -1, -1), (1, -1, -1), (1, -1, 1)]]
        normals = [vec3 x y z | normal@(x, y, z) <- [
            (0, 0, 1), (0, 0, 1), (0, 0, 1), (0, 0, 1),
            (1, 0, 0), (1, 0, 0), (1, 0, 0), (1, 0, 0),
            (0, 0, -1), (0, 0, -1), (0, 0, -1), (0, 0, -1),
            (-1, 0, 0), (-1, 0, 0), (-1, 0, 0), (-1, 0, 0),
            (0, 1, 0), (0, 1, 0), (0, 1, 0), (0, 1, 0),
            (0, -1, 0), (0, -1, 0), (0, -1, 0), (0, -1, 0)]]
        texcoords = [vec2 x y | texcoord@(x, y) <- [
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1)]]
        vertexCount = length positions
    in
        XFrame $ fromFlatList (D24 :* U) (Vertex 0 0 0 0) [Vertex (positions !! i) (normals !! i) (vec4 1 1 1 1) (texcoords !! i) | i <- [0..(vertexCount - 1)]]

cubeIndices :: DataFrameAtLeastThree Word32
cubeIndices = atLeastThree . fromList $ [ 0, 2, 1, 0, 3, 2,
                                          4, 6, 5, 4, 7, 6,
                                          8, 10, 9, 8, 11, 10,
                                          12, 14, 13, 12, 15, 14,
                                          16, 18, 17, 16, 19, 18,
                                          20, 22, 21, 20, 23, 22 ]

vertexInputBindDescription :: VkVertexInputBindingDescription
vertexInputBindDescription = createVk @VkVertexInputBindingDescription
    $  set @"binding" 0
    &* set @"stride"  (bSizeOf @Vertex undefined)
    &* set @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX

-- We can use DataFrames to keep several vulkan structures in a contiguous
-- memory areas, so that we can pass a pointer to a DataFrame directly into
-- a vulkan function with no copy.
--
-- However, we must make sure the created DataFrame is pinned!
vertexInputAttributeDescriptions :: Vector VkVertexInputAttributeDescription 4
vertexInputAttributeDescriptions = ST.runST $ do
    mv <- ST.newPinnedDataFrame
    ST.writeDataFrame mv 0 . scalar $ createVk
        $  set @"location" 0
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"pos" @Vertex undefined)
    ST.writeDataFrame mv 1 . scalar $ createVk
        $  set @"location" 1
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"normal" @Vertex undefined)
    ST.writeDataFrame mv 2 . scalar $ createVk
        $  set @"location" 2
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R8G8B8A8_UNORM
        &* set @"offset" (bFieldOffsetOf @"color" @Vertex undefined)
    ST.writeDataFrame mv 3 . scalar $ createVk
        $  set @"location" 3
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"texCoord" @Vertex undefined)
    ST.unsafeFreezeDataFrame mv


createGeometryData :: VkPhysicalDevice
                   -> VkDevice
                   -> VkQueue
                   -> VkCommandPool
                   -> Text.Text
                   -> DataFrameAtLeastThree Vertex
                   -> DataFrameAtLeastThree Word32
                   -> IO GeometryData
createGeometryData physicalDevice device graphicsQueue commandPool geometryName vertices indices = do
    logInfo $ "createGeometryBuffer : " ++ (Text.unpack geometryName)
    (vertexBufferMemory, vertexBuffer) <- createVertexBuffer physicalDevice device graphicsQueue commandPool vertices
    (indexBufferMemory, indexBuffer) <- createIndexBuffer physicalDevice device graphicsQueue commandPool indices
    return GeometryData
        { _geometryName = geometryName
        , _vertexBufferMemory = vertexBufferMemory
        , _vertexBuffer = vertexBuffer
        , _indexBufferMemory = indexBufferMemory
        , _indexBuffer = indexBuffer
        , _vertexIndexCount = (fromIntegral $ dataFrameLength indices)
        }

destroyGeometryData :: VkDevice -> GeometryData -> IO ()
destroyGeometryData device geometryBuffer = do
    logInfo "destroyGeometryData"
    destroyBuffer device (_vertexBuffer geometryBuffer) (_vertexBufferMemory geometryBuffer)
    destroyBuffer device (_indexBuffer geometryBuffer) (_indexBufferMemory geometryBuffer)


createVertexBuffer :: VkPhysicalDevice
                   -> VkDevice
                   -> VkQueue
                   -> VkCommandPool
                   -> DataFrameAtLeastThree Vertex
                   -> IO (VkDeviceMemory, VkBuffer)
createVertexBuffer physicalDevice device graphicsQueue commandPool (XFrame vertices) = do
    let bufferSize = bSizeOf vertices
        bufferUsageFlags = (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
        memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    logInfo $ "createVertexBuffer : bufferSize " ++ show bufferSize

    -- create temporary staging buffer
    let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
        stagingBufferMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (stagingBufferMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingBufferMemoryPropertyFlags

    -- upload data
    stagingDataPtr <- allocaPeek $ vkMapMemory device stagingBufferMemory 0 bufferSize VK_ZERO_FLAGS
    poke (castPtr stagingDataPtr) vertices
    vkUnmapMemory device stagingBufferMemory

    -- create vertex buffer & copy
    (vertexBufferMemory, vertexBuffer) <- createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags
    copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer bufferSize

    -- destroy temporary staging buffer
    destroyBuffer device stagingBuffer stagingBufferMemory

    return (vertexBufferMemory, vertexBuffer)


createIndexBuffer :: VkPhysicalDevice
                  -> VkDevice
                  -> VkQueue
                  -> VkCommandPool
                  -> DataFrameAtLeastThree Word32
                  -> IO (VkDeviceMemory, VkBuffer)
createIndexBuffer physicalDevice device graphicsQueue commandPool (XFrame indices) = do
    let bufferSize = bSizeOf indices
        bufferUsageFlags = (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
        memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    logInfo $ "createIndexBuffer : bufferSize " ++ show bufferSize

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

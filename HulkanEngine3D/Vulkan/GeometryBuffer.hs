{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module HulkanEngine3D.Vulkan.GeometryBuffer
  ( Vertex (..)
  , Tri (..)
  , GeometryData(..)
  , atLeastThree
  , dataFrameLength
  , triangleToFaceIndices
  , faceToTriangles
  , rectVertices
  , rectIndices
  , vertexInputBindDescription
  , vertexInputAttributeDescriptions
  , createGeometryData
  , destroyGeometryData
  , createVertexBuffer
  , createIndexBuffer
  ) where

import GHC.Generics (Generic)
import qualified Control.Monad.ST as ST
import Data.Bits
import qualified Data.Text as Text
import Foreign.Ptr (castPtr)
import Foreign.Storable
import Codec.Wavefront
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
  { pos      :: Vec3f
  , color    :: Vec3f
  , texCoord :: Vec2f
  } deriving (Eq, Ord, Show, Generic)

instance PrimBytes Vertex

data Tri = Tri {-# UNPACK #-}!FaceIndex
               {-# UNPACK #-}!FaceIndex
               {-# UNPACK #-}!FaceIndex

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

-- reversal here for correct culling in combination with the (-y) below
triangleToFaceIndices :: Tri -> [FaceIndex]
triangleToFaceIndices (Tri a b c) = [c, b, a]

faceToTriangles :: Face -> [Tri]
faceToTriangles (Face a b c []) = [Tri a b c]
faceToTriangles (Face a b c is) = pairwise (Tri a) (b:c:is)
  where pairwise f xs = zipWith f xs (tail xs)

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
rectVertices :: DataFrame Vertex '[XN 3]
rectVertices = XFrame $
    square
    `appendDF`
    withPos (+ vec4 0 0 0.5 0) square
    `appendDF`
    withPos (\p -> p %* rotateX (pi/2) + vec4 0 0 (-0.5) 0) square
  where
    square :: Vector Vertex 4
    square = fromFlatList (D4 :* U) (Vertex 0 0 0) -- default point for type safety
      [  -- rectangle
          --     coordinate                  color        texture coordinate
        Vertex (vec3 (-0.5) (-0.5) 0) (vec3 1 0 0) (vec2 0 0)
      , Vertex (vec3   0.4  (-0.5) 0) (vec3 0 1 0) (vec2 1 0)
      , Vertex (vec3   0.4    0.4  0) (vec3 0 0 1) (vec2 1 1)
      , Vertex (vec3 (-0.5)   0.4  0) (vec3 1 1 1) (vec2 0 1)
      ]
    withPos :: (Vec4f -> Vec4f) -> Vector Vertex 4 -> Vector Vertex 4
    withPos f = ewmap (\(S v) -> S v { pos = fromHom . f . toHomPoint $ pos v })

rectIndices :: DataFrame Word32 '[XN 3]
rectIndices = atLeastThree $ fromList $
  oneRectIndices
  ++
  map (+4) oneRectIndices
  ++
  map (+8) oneRectIndices
  where
    -- indices for one rectangle
    oneRectIndices = [0, 3, 2, 2, 1, 0]

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
vertexInputAttributeDescriptions :: Vector VkVertexInputAttributeDescription 3
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
        &* set @"offset" (bFieldOffsetOf @"color" @Vertex undefined)
                          -- Now we can use bFieldOffsetOf derived
                          -- in PrimBytes via Generics. How cool is that!
    ST.writeDataFrame mv 2 . scalar $ createVk
        $  set @"location" 2
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"texCoord" @Vertex undefined)
    ST.unsafeFreezeDataFrame mv


createGeometryData :: VkPhysicalDevice
                         -> VkDevice
                         -> VkQueue
                         -> VkCommandPool
                         -> Text.Text
                         -> DataFrame Vertex '[XN 3]
                         -> DataFrame Word32 '[XN 3]
                         -> IO GeometryData
createGeometryData physicalDevice device graphicsQueue commandPool geometryName vertices indices = do
    logInfo $ "createGeometryBuffer : " ++ (Text.unpack geometryName)
    (vertexBufferMemory, vertexBuffer) <- createVertexBuffer physicalDevice device graphicsQueue commandPool vertices
    (indexBufferMemory, indexBuffer) <- createIndexBuffer physicalDevice device graphicsQueue commandPool indices
    return GeometryData { _geometryName = geometryName
                              , _vertexBufferMemory = vertexBufferMemory
                              , _vertexBuffer = vertexBuffer
                              , _indexBufferMemory = indexBufferMemory
                              , _indexBuffer = indexBuffer
                              , _vertexIndexCount = (fromIntegral $ dataFrameLength indices) }

destroyGeometryData :: VkDevice -> GeometryData -> IO ()
destroyGeometryData device geometryBuffer = do
    logInfo "destroyGeometryData"
    destroyBuffer device (_vertexBuffer geometryBuffer) (_vertexBufferMemory geometryBuffer)
    destroyBuffer device (_indexBuffer geometryBuffer) (_indexBufferMemory geometryBuffer)


createVertexBuffer :: VkPhysicalDevice
                   -> VkDevice
                   -> VkQueue
                   -> VkCommandPool
                   -> DataFrame Vertex '[XN 3]
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
                  -> DataFrame Word32 '[XN 3]
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

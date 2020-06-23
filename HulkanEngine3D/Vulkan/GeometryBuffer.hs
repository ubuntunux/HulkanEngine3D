{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module HulkanEngine3D.Vulkan.GeometryBuffer where

import GHC.Generics (Generic)
import qualified Control.Monad.ST as ST
import Data.Bits
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Foreign.Ptr (castPtr)
import Foreign.Storable
import qualified Data.DList as DList

import Data.Aeson
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import qualified Numeric.DataFrame.ST as ST
import qualified Numeric.DataFrame as DF
import Numeric.DataFrame
import Numeric.Dimensions

import HulkanEngine3D.Vulkan.Buffer
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Utilities.BoundingBox
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math

data VertexData = VertexData
    { _vertexPosition :: Vec3f
    , _vertexNormal :: Vec3f
    , _vertexTanget :: Vec3f
    , _vertexColor :: Scalar Word32
    , _vertexTexCoord :: Vec2f
    } deriving (Eq, Ord, Show, Generic)

instance PrimBytes VertexData

data GeometryCreateInfo = GeometryCreateInfo
    { _geometryCreateInfoVertices :: DataFrameAtLeastThree VertexData
    , _geometryCreateInfoIndices :: DataFrameAtLeastThree Word32
    , _geometryCreateInfoBoundingBox :: BoundingBox
    } deriving (Eq, Show, Generic)

instance ToJSON GeometryCreateInfo where
    toJSON (GeometryCreateInfo vertices indices boundingBox) = do
        object [ "vertices" .= (show $ map (\(XFrame x) -> unScalar x) $ dataFrameToList vertices)
               , "indices" .= (map (unScalar . fromIntegral) $ dataFrameToList indices::[Word32])
               , "boundingBox" .= boundingBox
               ]

    toEncoding (GeometryCreateInfo vertices indices boundingBox) =
        pairs ( "vertices" .= (show $ map (\(XFrame x) -> unScalar x) $ dataFrameToList vertices)
              <> "indices" .= (map (unScalar . fromIntegral) $ dataFrameToList indices::[Word32])
              <> "boundingBox" .= boundingBox
              )

instance FromJSON GeometryCreateInfo where
    parseJSON (Object v) = do
--            name <- v .: "vertices"
--            age <- v .: "indices"
--            boundingBox <- v .: "boundingBox"
            return $ cubeGeometryCreateInfos !! 0
    parseJSON _ = error ""

data GeometryData = GeometryData
    { _geometryName :: Text.Text
    , _vertexBufferMemory :: VkDeviceMemory
    , _vertexBuffer :: VkBuffer
    , _indexBufferMemory :: VkDeviceMemory
    , _indexBuffer :: VkBuffer
    , _vertexIndexCount :: Word32
    , _geometryBoundingBox :: BoundingBox
    } deriving (Eq, Show, Generic)

defaultVertexData :: VertexData
defaultVertexData = VertexData 0 0 0 0 0

vertexInputBindDescription :: VkVertexInputBindingDescription
vertexInputBindDescription = createVk @VkVertexInputBindingDescription
    $  set @"binding" 0
    &* set @"stride"  (bSizeOf @VertexData undefined)
    &* set @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX

-- We can use DataFrames to keep several vulkan structures in a contiguous
-- memory areas, so that we can pass a pointer to a DataFrame directly into
-- a vulkan function with no copy.
--
-- However, we must make sure the created DataFrame is pinned!
vertexInputAttributeDescriptions :: DF.Vector VkVertexInputAttributeDescription 5
vertexInputAttributeDescriptions = ST.runST $ do
    mv <- ST.newPinnedDataFrame
    ST.writeDataFrame mv (Idx 0 :* U) . scalar $ createVk
        $  set @"location" 0
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"_vertexPosition" @VertexData undefined)
    ST.writeDataFrame mv (Idx 1 :* U) . scalar $ createVk
        $  set @"location" 1
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"_vertexNormal" @VertexData undefined)
    ST.writeDataFrame mv (Idx 2 :* U) . scalar $ createVk
        $  set @"location" 2
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"_vertexTanget" @VertexData undefined)
    ST.writeDataFrame mv (Idx 3 :* U) . scalar $ createVk
        $  set @"location" 3
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R8G8B8A8_UNORM
        &* set @"offset" (bFieldOffsetOf @"_vertexColor" @VertexData undefined)
    ST.writeDataFrame mv (Idx 4 :* U) . scalar $ createVk
        $  set @"location" 4
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"_vertexTexCoord" @VertexData undefined)
    ST.unsafeFreezeDataFrame mv


createGeometryData :: VkPhysicalDevice
                   -> VkDevice
                   -> VkQueue
                   -> VkCommandPool
                   -> Text.Text
                   -> GeometryCreateInfo
                   -> IO GeometryData
createGeometryData physicalDevice device graphicsQueue commandPool geometryName geometryCreateInfo = do
    logInfo $ "createGeometryBuffer : " ++ (Text.unpack geometryName)
    (vertexBufferMemory, vertexBuffer) <- createVertexBuffer physicalDevice device graphicsQueue commandPool (_geometryCreateInfoVertices geometryCreateInfo)
    (indexBufferMemory, indexBuffer) <- createIndexBuffer physicalDevice device graphicsQueue commandPool (_geometryCreateInfoIndices geometryCreateInfo)
    return GeometryData
        { _geometryName = geometryName
        , _vertexBufferMemory = vertexBufferMemory
        , _vertexBuffer = vertexBuffer
        , _indexBufferMemory = indexBufferMemory
        , _indexBuffer = indexBuffer
        , _vertexIndexCount = (fromIntegral . dataFrameLength $ _geometryCreateInfoIndices geometryCreateInfo)
        , _geometryBoundingBox = _geometryCreateInfoBoundingBox geometryCreateInfo
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
                   -> DataFrameAtLeastThree VertexData
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

{-
    Note: This point can also be considered as the vector starting from the origin to pi.
    Writting this equation for the points p1, p2 and p3 give :
        p1 = u1 * T + v1 * B
        p2 = u2 * T + v2 * B
        p3 = u3 * T + v3 * B
    Texture/World space relation

    With equation manipulation (equation subtraction), we can write :
        p2 - p1 = (u2 - u1) * T + (v2 - v1) * B
        p3 - p1 = (u3 - u1) * T + (v3 - v1) * B

    By resolving this system :
        Equation of Tangent:
            (v3 - v1) * (p2 - p1) = (v3 - v1) * (u2 - u1) * T + (v3 - v1) * (v2 - v1) * B
            (v2 - v1) * (p3 - p1) = (v2 - v1) * (u3 - u1) * T + (v2 - v1) * (v3 - v1) * B

        Equation of Binormal:
            (u3 - u1) * (p2 - p1) = (u3 - u1) * (u2 - u1) * T + (u3 - u1) * (v2 - v1) * B
            (u2 - u1) * (p3 - p1) = (u2 - u1) * (u3 - u1) * T + (u2 - u1) * (v3 - v1) * B


    And we finally have the formula of T and B :
        T = ((v3 - v1) * (p2 - p1) - (v2 - v1) * (p3 - p1)) / ((u2 - u1) * (v3 - v1) - (u3 - u1) * (v2 - v1))
        B = ((u3 - u1) * (p2 - p1) - (u2 - u1) * (p3 - p1)) / -((u2 - u1) * (v3 - v1) - (u3 - u1) * (v2 - v1))

    Equation of N:
        N = cross(T, B)
-}
computeTangent :: Vector.Vector Vec3f -> Vector.Vector Vec3f -> Vector.Vector Vec2f -> Vector.Vector (Scalar Word32) -> (Vector.Vector Vec3f)
computeTangent positions normals texcoords indices =
    let vertexCount = length positions
        indexCount = length indices
        tangentList = DList.foldr (\i acc -> DList.append (computeTangent' i positions texcoords normals) acc) DList.empty (DList.fromList [0,3..(indexCount - 1)])
        tangentMap = Map.fromList $ DList.toList tangentList
        keys = DList.map (\(index, tangent) -> index) tangentList
    in
        Vector.fromList . DList.toList $ DList.map (\key -> fromJust $ Map.lookup key tangentMap) keys
    where
        computeTangent' :: Int -> Vector.Vector Vec3f -> Vector.Vector Vec2f -> Vector.Vector Vec3f -> DList.DList (Int, Vec3f)
        computeTangent' i positions texcoords normals =
            let i0 = fromIntegral . unScalar $ indices Vector.! i
                i1 = fromIntegral . unScalar $ indices Vector.! (i + 1)
                i2 = fromIntegral . unScalar $ indices Vector.! (i + 2)
                deltaPos_0_1 = (positions Vector.! i1) - (positions Vector.! i0)
                deltaPos_0_2 = (positions Vector.! i2) - (positions Vector.! i0)
                deltaUV_0_1 = (texcoords Vector.! i1) - (texcoords Vector.! i0)
                deltaUV_0_2 = (texcoords Vector.! i2) - (texcoords Vector.! i0)
                S r = (deltaUV_0_1 .! Idx 0) * (deltaUV_0_2 .! Idx 1) - (deltaUV_0_1 .! Idx 1) * (deltaUV_0_2 .! Idx 0)
                r' = if r /= 0.0 then (1.0 / r) else 0.0
                tangent = safeNormalize $ (deltaPos_0_1 * (fromScalar $ deltaUV_0_2 .! Idx 1) - deltaPos_0_2 * (fromScalar $ deltaUV_0_1 .! Idx 1)) * (fromScalar $ scalar r')
                avg_normal = safeNormalize $ (normals Vector.! i0 + normals Vector.! i1 + normals Vector.! i2)
                resultTangent =
                    if 0.0 == (dot tangent tangent) then
                        cross avg_normal world_up
                    else
                        tangent
            in
                DList.fromList [(i0, resultTangent), (i1, resultTangent), (i2, resultTangent)]

quadGeometryCreateInfos :: [GeometryCreateInfo]
quadGeometryCreateInfos =
    let positionList = [vec3 -1.0 -1.0 0.0, vec3 1.0 -1.0 0.0, vec3 1.0 1.0 0.0, vec3 -1.0 1.0 0.0]
        positions = Vector.fromList positionList
        vertexCount = length positions
        normals = Vector.replicate vertexCount $ vec3 0 1 0
        vertexColor = getColor32 255 255 255 255
        texCoords = Vector.fromList [vec2 0 0, vec2 1 0, vec2 1 1, vec2 0 1]
        indices = Vector.fromList [0, 3, 2, 2, 1, 0] :: Vector.Vector (Scalar Word32)
        tangents = computeTangent positions normals texCoords indices
        vertices = [VertexData (positions Vector.! i) (normals Vector.! i) (tangents Vector.! i) vertexColor (texCoords Vector.! i) | i <- [0..(vertexCount - 1)]]
    in
        [ GeometryCreateInfo
            { _geometryCreateInfoVertices = XFrame $ DF.fromFlatList (D4 :* U) defaultVertexData vertices
            , _geometryCreateInfoIndices = atLeastThree . DF.fromList $ (Vector.toList indices)
            , _geometryCreateInfoBoundingBox = calcBoundingBox positionList
            }
        ]

cubeGeometryCreateInfos :: [GeometryCreateInfo]
cubeGeometryCreateInfos =
    let vertexColor = getColor32 255 255 255 255
        positionList = [vec3 x y z | (x, y, z) <- [
            (-1, 1, 1), (-1, -1, 1), (1, -1, 1), (1, 1, 1),
            (1, 1, 1), (1, -1, 1), (1, -1, -1), (1, 1, -1),
            (1, 1, -1), (1, -1, -1), (-1, -1, -1), (-1, 1, -1),
            (-1, 1, -1), (-1, -1, -1), (-1, -1, 1), (-1, 1, 1),
            (-1, 1, -1), (-1, 1, 1), (1, 1, 1), (1, 1, -1),
            (-1, -1, 1), (-1, -1, -1), (1, -1, -1), (1, -1, 1)]]
        positions = Vector.fromList positionList
        normals = Vector.fromList [vec3 x y z | (x, y, z) <- [
            (0, 0, 1), (0, 0, 1), (0, 0, 1), (0, 0, 1),
            (1, 0, 0), (1, 0, 0), (1, 0, 0), (1, 0, 0),
            (0, 0, -1), (0, 0, -1), (0, 0, -1), (0, 0, -1),
            (-1, 0, 0), (-1, 0, 0), (-1, 0, 0), (-1, 0, 0),
            (0, 1, 0), (0, 1, 0), (0, 1, 0), (0, 1, 0),
            (0, -1, 0), (0, -1, 0), (0, -1, 0), (0, -1, 0)]]
        texCoords = Vector.fromList [vec2 x y | (x, y) <- [
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1),
            (0, 1), (0, 0), (1, 0), (1, 1)]]
        indexList = [ 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6, 8, 10, 9, 8, 11, 10, 12, 14, 13, 12, 15, 14, 16, 18, 17, 16, 19, 18, 20, 22, 21, 20, 23, 22 ] :: [Scalar Word32]
        indices = Vector.fromList indexList
        tangents = computeTangent positions normals texCoords indices
        vertexCount = length positions
        vertices = [VertexData (positions Vector.! i) (normals Vector.! i) (tangents Vector.! i) vertexColor (texCoords Vector.! i) | i <- [0..(vertexCount - 1)]]
    in
        [ GeometryCreateInfo
            { _geometryCreateInfoVertices = XFrame $ DF.fromFlatList (D24 :* U) defaultVertexData vertices
            , _geometryCreateInfoIndices =  atLeastThree . DF.fromList $ indexList
            , _geometryCreateInfoBoundingBox = calcBoundingBox positionList
            }
        ]
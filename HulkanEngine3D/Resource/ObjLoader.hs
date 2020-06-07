{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HulkanEngine3D.Resource.ObjLoader
    ( loadMesh
    ) where

import Control.Monad
import qualified Codec.Wavefront as Wavefront
import qualified Data.List as List
import qualified Data.Vector as Vector
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Set as Set
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import Numeric.DataFrame
--import Numeric.Dimensions

import HulkanEngine3D.Utilities.BoundingBox
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.GeometryBuffer

data Triangle = Triangle {-# UNPACK #-}!Wavefront.FaceIndex
                         {-# UNPACK #-}!Wavefront.FaceIndex
                         {-# UNPACK #-}!Wavefront.FaceIndex

loadMesh :: FilePath -> IO [GeometryCreateInfo]
loadMesh file = do
    logInfo $ "Loading mesh: " ++ file
    obj <- either throwVKMsg pure =<< Wavefront.fromFile file
    objVertices obj

-- reversal here for correct culling in combination with the (-y) below
triangleToFaceIndices :: Triangle -> [Wavefront.FaceIndex]
triangleToFaceIndices (Triangle a b c) = [c, b, a]

faceToTriangles :: Wavefront.Face -> [Triangle]
faceToTriangles (Wavefront.Face a b c []) = [Triangle a b c]
faceToTriangles (Wavefront.Face a b c is) = pairwise (Triangle a) (b:c:is)
    where pairwise f xs = zipWith f xs (tail xs)

objVertices :: Wavefront.WavefrontOBJ -> IO [GeometryCreateInfo]
objVertices Wavefront.WavefrontOBJ {..} = do
        forM groupIndicesPair $ \groupIndexPair@(startIndex, endIndex) -> do
            let triangles = concatMap (\i -> faceToTriangles $ Wavefront.elValue (objFaces Vector.! i)) [startIndex..endIndex]
                faceIndices = concatMap triangleToFaceIndices triangles
                allObjVertices = map (convertToDataFrame objLocations objNormals objTexCoords) faceIndices
                objVertexSet = Set.fromList allObjVertices
                uniqueObjVertexList = Set.toList objVertexSet
                (positions, normals, texCoords) = unzip3 uniqueObjVertexList
                vertexIndices = map (fromIntegral . flip Set.findIndex objVertexSet) allObjVertices
                vertexCount = length positions
                tangents = computeTangent positions normals texCoords vertexIndices
                vertices = [scalar $ VertexData (positions !! i) (normals !! i) (tangents !! i) vertexColor (texCoords !! i) | i <- [0..(vertexCount - 1)]]
                result = GeometryCreateInfo
                    { _geometryCreateInfoVertices = atLeastThree . fromList $ vertices
                    , _geometryCreateInfoIndices = atLeastThree . fromList $ vertexIndices
                    , _geometryCreateInfoBoundingBox = calcBoundingBox $ map (\(S vertex) -> _vertexPosition vertex) vertices
                    }
            return result
    where
        vertexColor = getColor32 255 255 255 255
        allFaceList = toList objFaces
        groups = List.group . map (\face -> (Wavefront.elObject face, Wavefront.elMtl face)) $ allFaceList
        groupIndices = foldl (\acc xs -> acc ++ [last acc + length xs]) [0] groups
        groupIndicesPair = zipWith (\x y -> (x, y-1)) groupIndices (tail groupIndices) -- (startIndex, endIndex)
        convertToDataFrame :: Vector.Vector Wavefront.Location
                           -> Vector.Vector Wavefront.Normal
                           -> Vector.Vector Wavefront.TexCoord
                           -> Wavefront.FaceIndex
                           -> (Vec3f, Vec3f, Vec2f)
        convertToDataFrame objLocs objNorms objTexCs faceIndex@Wavefront.FaceIndex {..} =
            ( (\(Wavefront.Location x y z _) -> vec3 x y z) $ objLocs Vector.! fromIntegral (faceLocIndex - 1)
            , (\(Wavefront.Normal x y z) -> vec3 x y z) $ objNorms Vector.! fromIntegral (fromJust faceNorIndex - 1)
            , (\(Wavefront.TexCoord x y _) -> vec2 x y) $ objTexCs Vector.! fromIntegral (fromJust faceTexCoordIndex - 1)
            )
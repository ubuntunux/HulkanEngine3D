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

import qualified Codec.Wavefront as Wavefront
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Set as Set
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import Numeric.DataFrame
import Numeric.Dimensions

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
    geometryCreateInfo <- objVertices obj
    return [geometryCreateInfo]

-- reversal here for correct culling in combination with the (-y) below
triangleToFaceIndices :: Triangle -> [Wavefront.FaceIndex]
triangleToFaceIndices (Triangle a b c) = [c, b, a]

faceToTriangles :: Wavefront.Face -> [Triangle]
faceToTriangles (Wavefront.Face a b c []) = [Triangle a b c]
faceToTriangles (Wavefront.Face a b c is) = pairwise (Triangle a) (b:c:is)
    where pairwise f xs = zipWith f xs (tail xs)

objVertices :: Wavefront.WavefrontOBJ -> IO GeometryCreateInfo
objVertices Wavefront.WavefrontOBJ {..}
    | XFrame objLocs  <- fromList . map (\(Wavefront.Location x y z _) -> vec3 x y z) $ toList objLocations
    , XFrame objNorms  <- fromList . map (\(Wavefront.Normal x y z) -> vec3 x y z) $ toList objNormals
    , XFrame objTexCs <- fromList . map (\(Wavefront.TexCoord r s _) -> vec2 r s) $ toList objTexCoords
      -- the two lines below let GHC know the value length of objLocs and objTexCs
      -- at the type level; we need this for the mkVertex function below.
    , D :* numObjLocs <- inSpaceOf dims objLocs
    , D :* numObjNorms <- inSpaceOf dims objNorms
    , D :* numObjTexCs <- inSpaceOf dims objTexCs
    , allObjVertices <- map (convertToDataFrame objLocs objNorms objTexCs) faceIndices
    , objVertexSet <- Set.fromList allObjVertices
    , uniqueObjVertexList <- Set.toList objVertexSet
    , (positions, normals, texCoords) <- unzip3 uniqueObjVertexList
    , vertexIndices <- map (scalar . fromIntegral . flip Set.findIndex objVertexSet) allObjVertices
    , tangents <- computeTangent positions normals texCoords vertexIndices
    , vertexCount <- length positions
    , vertices <- [scalar $ VertexData (positions !! i) (normals !! i) (tangents !! i) vertexColor (texCoords !! i) | i <- [0..(vertexCount - 1)]]
      = do
        --let tangentList = computeTangent uniqueVertexList vertexIndices
        return GeometryCreateInfo
            { _geometryCreateInfoVertices = atLeastThree . fromList $ vertices
            , _geometryCreateInfoIndices = atLeastThree . fromList $ vertexIndices
            , _geometryCreateInfoBoundingBox = calcBoundingBox $ map (\(S vertex) -> _vertexPosition vertex) vertices
            }
    | otherwise = error "objVertices: impossible arguments"
    where
        vertexColor = getColor32 255 255 255 255
        triangles = concatMap (\face -> faceToTriangles $ Wavefront.elValue face) $ toList objFaces
        faceIndices = concatMap triangleToFaceIndices triangles
        {- Note, we need to substract 1 from all indices, because Wavefron OBJ indices
           are 1-based (rather than 0-based indices of vector or easytensor packages).

           More info: http://www.martinreddy.net/gfx/3d/OBJ.spec
           "Each of these types of vertices is numbered separately, starting with 1"
         -}
        convertToDataFrame :: (KnownDim l, KnownDim n, KnownDim m)
                           => Matrix Float l 3
                           -> Matrix Float n 3
                           -> Matrix Float m 2
                           -> Wavefront.FaceIndex
                           -> (Vec3f, Vec3f, Vec2f)
        convertToDataFrame objLocs objNorms objTexCs faceIndex@Wavefront.FaceIndex {..} =
          ( objLocs ! fromIntegral (faceLocIndex - 1)
          , objNorms ! fromIntegral (fromJust faceNorIndex - 1)
          , objTexCs ! fromIntegral (fromJust faceTexCoordIndex - 1)
          )

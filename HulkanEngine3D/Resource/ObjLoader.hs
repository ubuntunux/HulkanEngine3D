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
    , allVertices <- map (scalar . mkVertex objLocs objNorms objTexCs) faceIndices
    , uniqueVertexSet <- Set.fromList allVertices
    , uniqueVertexList <- Set.toList uniqueVertexSet
    , vertexIndices <- map (scalar . fromIntegral . flip Set.findIndex uniqueVertexSet) allVertices
      = do
        --let tangentList = computeTangent uniqueVertexList vertexIndices
        return GeometryCreateInfo
            { _geometryCreateInfoVertices = atLeastThree . fromList $ uniqueVertexList
            , _geometryCreateInfoIndices = atLeastThree . fromList $ vertexIndices
            , _geometryCreateInfoBoundingBox = calcBoundingBox $ map (\(S vertex) -> _vertexPosition vertex) uniqueVertexList
            }
    | otherwise = error "objVertices: impossible arguments"
    where
        triangles = concatMap (\face -> faceToTriangles $ Wavefront.elValue face) $ toList objFaces
        faceIndices = concatMap triangleToFaceIndices triangles
        {- Note, we need to substract 1 from all indices, because Wavefron OBJ indices
           are 1-based (rather than 0-based indices of vector or easytensor packages).

           More info: http://www.martinreddy.net/gfx/3d/OBJ.spec
           "Each of these types of vertices is numbered separately, starting with 1"
         -}
        mkVertex :: (KnownDim l, KnownDim n, KnownDim m)
                 => Matrix Float l 3
                 -> Matrix Float n 3
                 -> Matrix Float m 2
                 -> Wavefront.FaceIndex
                 -> VertexData
        mkVertex objLocs objNorms objTexCs faceIndex@Wavefront.FaceIndex {..} = VertexData
          { _vertexPosition = objLocs ! fromIntegral (faceLocIndex - 1)
          , _vertexNormal = objNorms ! fromIntegral (fromJust faceNorIndex - 1)
          , _vertexColor = getColor32 255 255 255 255
          , _vertexTexCoord = objTexCs ! fromIntegral (fromJust faceTexCoordIndex - 1)
          }

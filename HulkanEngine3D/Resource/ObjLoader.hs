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
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import Numeric.DataFrame
import Numeric.Dimensions

import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Vulkan.GeometryBuffer


loadMesh :: FilePath -> IO (DataFrame Vertex '[XN 3], DataFrame Word32 '[XN 3])
loadMesh file = do
    logInfo $ "Loading mesh: " ++ file
    obj <- either throwVKMsg pure =<< Wavefront.fromFile file
    (vertices, indices) <- objVertices obj
    return (vertices, indices)

-- reversal here for correct culling in combination with the (-y) below
triangleToFaceIndices :: Triangle -> [Wavefront.FaceIndex]
triangleToFaceIndices (Triangle a b c) = [c, b, a]

faceToTriangles :: Wavefront.Face -> [Triangle]
faceToTriangles (Wavefront.Face a b c []) = [Triangle a b c]
faceToTriangles (Wavefront.Face a b c is) = pairwise (Triangle a) (b:c:is)
    where pairwise f xs = zipWith f xs (tail xs)

objVertices :: Wavefront.WavefrontOBJ -> IO (DataFrame Vertex '[XN 3], DataFrame Word32 '[XN 3])
objVertices Wavefront.WavefrontOBJ {..}
    | XFrame objLocs  <- fromList . map fromLoc  $ toList objLocations
    , XFrame objTexCs <- fromList . map fromTexC $ toList objTexCoords
      -- the two lines below let GHC know the value length of objLocs and objTexCs
      -- at the type level; we need this for the mkVertex function below.
    , D :* numObjLocs <- inSpaceOf dims objLocs
    , D :* numObjTexCs <- inSpaceOf dims objTexCs
    , allVertices <- map (scalar . mkVertex objLocs objTexCs) faceIndices
    , vertSet <- Set.fromList allVertices
      = return
            ( atLeastThree . fromList $ Set.toList vertSet
            , atLeastThree . fromList $ map (fromIntegral . flip Set.findIndex vertSet) allVertices
            )
    | otherwise = error "objVertices: impossible arguments"
    where
        triangles = concatMap (\face -> faceToTriangles $ Wavefront.elValue face) $ toList objFaces
        faceIndices = concatMap triangleToFaceIndices triangles
        fromLoc (Wavefront.Location x y z _) = vec3 x y z
        fromTexC (Wavefront.TexCoord r s _) = vec2 r (1 - s)
        {- Note, we need to substract 1 from all indices, because Wavefron OBJ indices
           are 1-based (rather than 0-based indices of vector or easytensor packages).

           More info: http://www.martinreddy.net/gfx/3d/OBJ.spec
           "Each of these types of vertices is numbered separately, starting with 1"
         -}
        mkVertex :: ( KnownDim n, KnownDim m)
                 => Matrix Float n 3
                 -> Matrix Float m 2
                 -> Wavefront.FaceIndex
                 -> Vertex
        mkVertex objLocs objTexCs faceIndex@Wavefront.FaceIndex {..} = Vertex
          { pos      = objLocs ! fromIntegral (faceLocIndex - 1)
          , color    = vec3 1 1 1
          , texCoord = objTexCs ! fromIntegral (fromJust faceTexCoordIndex - 1)
          }

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Library.Resource.ObjLoader
  ( loadModel
  ) where

import Codec.Wavefront
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Set as Set
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import Numeric.DataFrame
import Numeric.Dimensions

import Library.Utilities.Logger
import Library.Utilities.System
import Library.Vulkan.Mesh


loadModel :: FilePath -> IO (DataFrame Vertex '[XN 3], DataFrame Word32 '[XN 3])
loadModel file = do
  logInfo $ "Loading model: " ++ file
  obj <- either throwVKMsg pure =<< Codec.Wavefront.fromFile file
  (vertices, indices) <- objVertices obj
  return (vertices, indices)

objVertices :: WavefrontOBJ -> IO (DataFrame Vertex '[XN 3], DataFrame Word32 '[XN 3])
objVertices WavefrontOBJ {..}
  | XFrame objLocs  <- fromList . map fromLoc  $ toList objLocations
  , XFrame objTexCs <- fromList . map fromTexC $ toList objTexCoords
    -- the two lines below let GHC know the value length of objLocs and objTexCs
    -- at the type level; we need this for the mkVertex function below.
  , D :* _ <- dims `inSpaceOf` objLocs
  , D :* _ <- dims `inSpaceOf` objTexCs
  , allVertices <- map (scalar . mkVertex objLocs objTexCs) faceIndices
  , vertSet <- Set.fromList allVertices
    = do        
        -- let vertex = Vertex { pos = (objLocs ! 0), color = vec3 1 1 1, texCoord = vec2 1 1 }
        -- let scalar_vertex = scalar vertex
        -- putStr "vertex : "
        -- print vertex
        -- putStr "scalar_vertex : "
        -- print scalar_vertex
        -- putStr "faceIndices !! 0 : "
        -- print $ faceIndices !! 0
        -- putStr "objLocs ! 0 : "
        -- print $ ((objLocs ! 0)::Vec3f)
        -- putStr "allVertices !! 0 : "
        -- print $ allVertices !! 0
        -- (XFrame vertises) <- return $ (atLeastThree . fromList $ [scalar_vertex, scalar_vertex, scalar_vertex])
        -- putStr "vertises : "
        -- print $ vertises
        return
          ( atLeastThree . fromList $ Set.toList vertSet
          , atLeastThree . fromList $ map (fromIntegral . flip Set.findIndex vertSet) allVertices
          )
  | otherwise = error "objVertices: impossible arguments"
  where
    triangles = concatMap (faceToTriangles . elValue) $ toList objFaces
    faceIndices = concatMap triangleToFaceIndices triangles
    fromLoc (Location x y z _) = vec3 x y z
    fromTexC (TexCoord r s _) = vec2 r (1 - s)
    {- Note, we need to substract 1 from all indices, because Wavefron OBJ indices
       are 1-based (rather than 0-based indices of vector or easytensor packages).

       More info: http://www.martinreddy.net/gfx/3d/OBJ.spec
       "Each of these types of vertices is numbered separately, starting with 1"
     -}
    mkVertex :: ( KnownDim n, KnownDim m)
             => Matrix Float n 3
             -> Matrix Float m 2
             -> FaceIndex 
             -> Vertex
    mkVertex objLocs objTexCs FaceIndex {..} = Vertex
      { pos      = objLocs ! fromIntegral (faceLocIndex - 1)
      , color    = vec3 1 1 1
      , texCoord = objTexCs ! fromIntegral (fromJust faceTexCoordIndex - 1)
      }

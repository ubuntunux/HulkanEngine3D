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
  ) where

import GHC.Generics (Generic)
import Codec.Wavefront
import Numeric.DataFrame


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
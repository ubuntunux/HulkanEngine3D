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
  (

  ) where

import           Codec.Wavefront
import qualified Control.Monad.ST                         as ST
import           Data.Foldable                            (toList)
import           Data.Maybe
import qualified Data.Set                                 as Set
import           GHC.Generics                             (Generic)
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Numeric.DataFrame
import qualified Numeric.DataFrame.ST                     as ST
import           Numeric.Dimensions


data Vertex = Vertex
  { pos      :: Vec3f
  , color    :: Vec3f
  , texCoord :: Vec2f
  } deriving (Eq, Ord, Show, Generic)

instance PrimBytes Vertex
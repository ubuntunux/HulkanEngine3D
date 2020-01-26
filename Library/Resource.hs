{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Library.Resource
  ( GeometryData(..)
  ) where


import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import Numeric.DataFrame

import Library.Vulkan.Mesh

class Resource a where

data GeometryData = GeometryData {
  vertices :: DataFrame Vertex '[XN 3],
  indices :: DataFrame Word32 '[XN 3]
} deriving Show

instance Resource GeometryData
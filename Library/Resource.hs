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
  ( Resource
  ) where


import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create.DataFrame ()
import Numeric.DataFrame

import Library.Vulkan.Mesh


data Resource = Resource {
  vertices :: DataFrame Vertex '[XN 3],
  indices :: DataFrame Word32 '[XN 3]
}
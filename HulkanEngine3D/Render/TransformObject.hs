{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}


module HulkanEngine3D.Render.TransformObject
  ( TransformObject (..)
  ) where


import GHC.Generics (Generic)
import Numeric.DataFrame
import Numeric.Dimensions

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger


data TransformObject = TransformObject
    { _position :: Vec3f
    , _rotation :: Vec3f
    , _scale :: Vec3f
    } deriving (Show, Generic)

instance PrimBytes TransformObject
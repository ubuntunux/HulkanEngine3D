{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}


module HulkanEngine3D.Render.TransformObject
  ( TransformObject (..)
  , TransformObjectInterface (..)
  ) where


import GHC.Generics (Generic)
import Numeric.DataFrame

import HulkanEngine3D.Utilities.Math


data TransformObject = TransformObject
    { _position :: Vec3f
    , _rotation :: Vec3f
    , _scale :: Vec3f
    , _matrix :: Mat44f
    , _invMatrix :: Mat44f
    } deriving (Show, Generic)

instance PrimBytes TransformObject


class TransformObjectInterface a where
    getDefailtTransformObject :: a -> a


instance TransformObjectInterface TransformObject where
    getDefailtTransformObject transformObject = TransformObject
        { _position = float3_zero
        , _rotation = float3_zero
        , _scale = fromScalar 1
        , _matrix = matrix4_indentity
        , _invMatrix = matrix4_indentity }
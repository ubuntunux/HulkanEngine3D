{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}


module HulkanEngine3D.Render.TransformObject
  ( TransformObjectData (..)
  , TransformObjectInterface (..)
  ) where

import Data.IORef
import GHC.Generics (Generic)
import Numeric.DataFrame

import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Utilities.System

data TransformObjectData = TransformObjectData
    { _position :: IORef Vec3f
    , _rotation :: Vec3f
    , _scale :: Vec3f
    , _matrix :: Mat44f
    , _invMatrix :: Mat44f
    } deriving (Show, Generic)


class TransformObjectInterface a where
    getDefaultTransformObjectData :: IO a


instance TransformObjectInterface TransformObjectData where
    getDefaultTransformObjectData = do
        position <- newIORef float3_zero
        return TransformObjectData
            { _position = position
            , _rotation = float3_zero
            , _scale = fromScalar 1
            , _matrix = matrix4_indentity
            , _invMatrix = matrix4_indentity }
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}


module HulkanEngine3D.Render.Camera
    ( CameraData (..)
    , CameraInterface (..)
    ) where

import HulkanEngine3D.Render.TransformObject


data CameraData = CameraData
    { _meterPerUnit :: Float
    , _near :: Float
    , _far :: Float
    , _fov :: Float
    , _aspect :: Float
    , _transformObject :: TransformObjectData
    } deriving (Show)


class CameraInterface a where
    getDefaultCameraData :: Float -> Float -> Float -> Float-> a
    updateCameraData :: a -> a

instance CameraInterface CameraData where
    getDefaultCameraData near far fov aspect = CameraData
        { _meterPerUnit = 1.0
        , _near = near
        , _far = far
        , _fov = fov
        , _aspect = aspect
        , _transformObject =  getDefaultTransformObjectData
        }

    updateCameraData cameraData = cameraData


{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NegativeLiterals    #-}

module HulkanEngine3D.Render.Camera
    ( CameraData (..)
    , CameraInterface (..)
    ) where

import Data.IORef
import Numeric.DataFrame
import HulkanEngine3D.Render.TransformObject


data CameraData = CameraData
    { _meterPerUnit :: IORef Float
    , _near :: IORef Float
    , _far :: IORef Float
    , _fov :: IORef Float
    , _aspect :: IORef Float
    , _transformObject :: TransformObjectData
    } deriving (Show)


class CameraInterface a where
    newCameraData :: Float -> Float -> Float -> Float-> IO a
    updateCameraData :: a -> a

instance CameraInterface CameraData where
    newCameraData near far fov aspect = do
        meterPerUnitRef <- newIORef 1.0
        nearRef <- newIORef near
        farRef <- newIORef far
        fovRef <- newIORef fov
        aspectRef <- newIORef aspect
        transformObjectData <- newTransformObjectData
        setPosition transformObjectData (vec3 0 0 10)
        return CameraData
            { _meterPerUnit = meterPerUnitRef
            , _near = nearRef
            , _far = farRef
            , _fov = fovRef
            , _aspect = aspectRef
            , _transformObject = transformObjectData
            }

    updateCameraData cameraData = cameraData


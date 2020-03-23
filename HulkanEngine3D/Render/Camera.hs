{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.Camera where

import Data.IORef
import qualified Data.Text as T
import Numeric.DataFrame
import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.TransformObject
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math


data CameraCreateData = CameraCreateData
    { meterPerUnit :: Float
    , near :: Float
    , far :: Float
    , fov :: Float
    , aspect :: Float
    , position :: Vec3f
    } deriving (Show)


getDefaultCameraCreateData :: CameraCreateData
getDefaultCameraCreateData = CameraCreateData
    { meterPerUnit = Constants.meterPerUnit
    , near = Constants.near
    , far = Constants.far
    , fov = Constants.fov
    , aspect = 1.0
    , position = vec3 0 0 0
    }


data CameraObjectData = CameraObjectData
    { _name :: IORef T.Text
    , _meterPerUnit :: IORef Float
    , _near :: IORef Float
    , _far :: IORef Float
    , _fov :: IORef Float
    , _aspect :: IORef Float
    , _projectionMatrix :: IORef Mat44f
    , _viewMatrix :: IORef Mat44f
    , _transformObject :: TransformObjectData
    } deriving (Show)


createCameraObjectData :: T.Text -> CameraCreateData -> IO CameraObjectData
createCameraObjectData name cameraCreateData = do
    logInfo $ "createCameraObjectData :: " ++ show name
    nameRef <- newIORef name
    meterPerUnitRef <- newIORef (meterPerUnit cameraCreateData)
    nearRef <- newIORef (near cameraCreateData)
    farRef <- newIORef (far cameraCreateData)
    fovRef <- newIORef (fov cameraCreateData)
    aspectRef <- newIORef (aspect cameraCreateData)
    projectionMatrixRef <- newIORef matrix4x4_indentity
    viewMatrixRef <- newIORef matrix4x4_indentity
    transformObjectData <- newTransformObjectData
    let cameraObjectData = CameraObjectData
            { _name = nameRef
            , _meterPerUnit = meterPerUnitRef
            , _near = nearRef
            , _far = farRef
            , _fov = fovRef
            , _aspect = aspectRef
            , _projectionMatrix = projectionMatrixRef
            , _viewMatrix = viewMatrixRef
            , _transformObject = transformObjectData
            }

    -- initialize
    setPosition transformObjectData (position cameraCreateData)
    updateProjectionMatrix cameraObjectData
    return cameraObjectData


updateCameraObjectData :: CameraObjectData -> IO ()
updateCameraObjectData cameraObjectData = do
    updateTransformObject (_transformObject cameraObjectData)
    viewMatrix <- readIORef (_inverseMatrix._transformObject $ cameraObjectData)
    writeIORef (_viewMatrix cameraObjectData) viewMatrix


updateProjectionMatrix :: CameraObjectData -> IO ()
updateProjectionMatrix cameraObjectData = do
    fov <- readIORef $ _fov cameraObjectData
    aspect <- readIORef $ _aspect cameraObjectData
    near <- readIORef $ _near cameraObjectData
    far <- readIORef $ _far cameraObjectData
    writeIORef (_projectionMatrix cameraObjectData) (projectionMatrix fov aspect near far)
    where
        -- ... which is a normal perspective projection matrix that maps the view space
        --     onto the clip space cube {x: -1..1, y: -1..1, z: -1..1}
        projectionMatrix fov aspect near far = (perspective near far (fov/360.0 * 2.0 * pi) aspect) %* clipSpace
        -- ... and a {clip space -> screen space} matrix that converts points into
        --     the vulkan screen space {x: -1..1, y: 1..-1, z: 0..1}
        clipSpace = DF4
            (DF4 1   0   0   0)
            (DF4 0 (-1)  0   0)
            (DF4 0   0  0.5  0)
            (DF4 0   0  0.5  1)
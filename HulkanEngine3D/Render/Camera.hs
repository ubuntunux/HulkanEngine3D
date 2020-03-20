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


data CameraCreateData = CameraCreateData
    { meterPerUnit :: Float
    , near :: Float
    , far :: Float
    , fov :: Float
    , aspect :: Float
    , position :: Vec3f
    } deriving (Show)

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
    transformObjectData <- newTransformObjectData
    setPosition transformObjectData (position cameraCreateData)
    return CameraObjectData
        { _name = nameRef
        , _meterPerUnit = meterPerUnitRef
        , _near = nearRef
        , _far = farRef
        , _fov = fovRef
        , _aspect = aspectRef
        , _transformObject = transformObjectData
        }

updateCameraObjectData :: CameraObjectData -> IO ()
updateCameraObjectData cameraObjectData = return ()


{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}


module HulkanEngine3D.Render.TransformObject
  ( TransformObjectData (..)
  , TransformObjectInterface (..)
  ) where

import Control.Monad (when)
import Data.IORef
import GHC.Generics (Generic)
import Numeric.DataFrame
import Numeric.Quaternion

import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System

data TransformObjectData = TransformObjectData
    { _updated :: IORef Bool

    , _front :: IORef Vec3f
    , _left :: IORef Vec3f
    , _up :: IORef Vec3f

    , _position :: IORef Vec3f
    , _rotation :: IORef Vec3f
    , _scale :: IORef Vec3f
    , _eulerToQuaternion :: IORef (Quater Float)
    , _quaternion :: IORef (Quater Float)
    , _fianlQuaternion :: IORef (Quater Float)

    , _prevPosition :: IORef Vec3f
    , _prevPositionStore :: IORef Vec3f
    , _prevRotation :: IORef Vec3f
    , _prevScale :: IORef Vec3f
    , _prevEulerToQuaternion :: IORef (Quater Float)
    , _prevQuaternion :: IORef (Quater Float)
    , _prevFianlQuaternion :: IORef (Quater Float)

    , _quaternionMatrix :: IORef Mat44f
    , _eulerMatrix :: IORef Mat44f
    , _rotationMatrix  :: IORef Mat44f

    , _matrix :: IORef Mat44f
    , _inverseMatrix :: IORef Mat44f
    , _prevMatrix :: IORef Mat44f
    , _prevInverseMatrix :: IORef Mat44f
    } deriving (Show, Generic)


class TransformObjectInterface a where
    getDefaultTransformObjectData :: IO a
    move :: a -> Vec3f -> Float -> IO ()
    moveLeft :: a -> Float -> IO ()
    moveUp :: a -> Float -> IO ()
    moveFront :: a -> Float -> IO ()
    updateTransformObject :: a -> IO ()


instance TransformObjectInterface TransformObjectData where
    getDefaultTransformObjectData = do
        updated <- newIORef True
        front <- newIORef world_front
        left <- newIORef world_left
        up <- newIORef world_up

        position <- newIORef float3_zero
        rotation <- newIORef float3_zero
        scale <- newIORef (getFloat3 1.0)
        eulerToQuaternion <- newIORef quaternion_identity
        quaternion <- newIORef quaternion_identity
        fianlQuaternion <- newIORef quaternion_identity

        prevPositionStore <- newIORef float3_zero
        prevPosition <- newIORef float3_zero
        prevRotation <- newIORef float3_zero
        prevScale <- newIORef (getFloat3 1.0)
        prevEulerToQuaternion <- newIORef quaternion_identity
        prevQuaternion <- newIORef quaternion_identity
        prevFianlQuaternion <- newIORef quaternion_identity

        quaternionMatrix <- newIORef matrix4x4_indentity
        eulerMatrix <- newIORef matrix4x4_indentity
        rotationMatrix  <- newIORef matrix4x4_indentity

        matrix <- newIORef matrix4x4_indentity
        inverseMatrix <- newIORef matrix4x4_indentity
        prevMatrix <- newIORef matrix4x4_indentity
        prevInverseMatrix <- newIORef matrix4x4_indentity

        return TransformObjectData
            { _updated = updated

            , _front = front
            , _left = left
            , _up = up

            , _position = position
            , _rotation = rotation
            , _scale = scale
            , _eulerToQuaternion = eulerToQuaternion
            , _quaternion = quaternion
            , _fianlQuaternion = fianlQuaternion

            , _prevPosition = prevPosition
            , _prevPositionStore = prevPositionStore
            , _prevRotation = prevRotation
            , _prevScale = prevScale
            , _prevEulerToQuaternion = prevEulerToQuaternion
            , _prevQuaternion = prevQuaternion
            , _prevFianlQuaternion = prevFianlQuaternion

            , _quaternionMatrix = quaternionMatrix
            , _eulerMatrix = eulerMatrix
            , _rotationMatrix  = rotationMatrix

            , _matrix = matrix
            , _inverseMatrix = inverseMatrix
            , _prevMatrix = prevMatrix
            , _prevInverseMatrix = prevInverseMatrix
            }

    move transformObjectData vector moveSpeed = do
         position <- readIORef (_position transformObjectData)
         writeIORef (_position transformObjectData) $ position + vector * (fromScalar . scalar $ moveSpeed)

    moveLeft transformObjectData moveSpeed = do
        leftVector <- readIORef (_left transformObjectData)
        move transformObjectData leftVector moveSpeed

    moveUp transformObjectData moveSpeed = do
        upVector <- readIORef (_up transformObjectData)
        move transformObjectData upVector moveSpeed

    moveFront transformObjectData moveSpeed = do
        frontVector <- readIORef (_front transformObjectData)
        move transformObjectData frontVector moveSpeed

    updateTransformObject transformObjectData@TransformObjectData {..} = do
        updateInverseMatrix <- pure True
        forceUpdate <- pure False

        prevUpdated <- readIORef _updated
        updated <- pure False
        rotationUpdate <- pure False

        position <- readIORef _position
        rotation <- readIORef _rotation
        scale <- readIORef _scale

        prevPosition <- readIORef _prevPosition
        prevRotation <- readIORef _prevRotation
        prevScale <- readIORef _prevScale

        updated <- if (forceUpdate || (position /= prevPosition))
            then do
                writeIORef _prevPositionStore prevPosition
                writeIORef _prevPosition position
                return True
            else
                return False

        when (prevUpdated || updated) $ do
            matrix <- readIORef _matrix
            writeIORef _prevMatrix matrix
            when updateInverseMatrix $ do
                inverseMatrix <- readIORef _inverseMatrix
                writeIORef _prevInverseMatrix inverseMatrix

        rotationMatrix <- readIORef _rotationMatrix

        when updated $ do
            writeIORef _matrix $ transform_matrix position rotationMatrix scale
            when updateInverseMatrix $ do
                writeIORef _inverseMatrix $ inverse_transform_matrix position rotationMatrix scale
        return ()
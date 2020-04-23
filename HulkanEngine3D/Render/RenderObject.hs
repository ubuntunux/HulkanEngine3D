{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE InstanceSigs           #-}

module HulkanEngine3D.Render.RenderObject where

import qualified Data.Text as T

import Numeric.DataFrame

import HulkanEngine3D.Render.Model
import HulkanEngine3D.Render.TransformObject
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System()

data StaticObjectCreateData = StaticObjectCreateData
    { _modelData' :: ModelData
    , _position' :: Vec3f
    } deriving Show

data StaticObjectData = StaticObjectData
    { _staticObjectName :: T.Text
    , _modelData :: ModelData
    , _transformObject :: TransformObjectData
    } deriving Show


class StaticObjectInterface a where
    createStaticObjectData :: T.Text -> StaticObjectCreateData -> IO a
    getModelData :: a -> ModelData
    getTransformObjectData :: a -> TransformObjectData
    updateStaticObjectData :: a -> IO ()

instance StaticObjectInterface StaticObjectData where
    createStaticObjectData :: T.Text -> StaticObjectCreateData -> IO StaticObjectData
    createStaticObjectData staticObjectName staticObjectCreateData = do
        logInfo $ "createStaticObjectData :: " ++ show staticObjectName
        transformObjectData <- newTransformObjectData
        setPosition transformObjectData $ _position' staticObjectCreateData
        return StaticObjectData
            { _staticObjectName = staticObjectName
            , _modelData = _modelData' staticObjectCreateData
            , _transformObject = transformObjectData
            }

    getModelData :: StaticObjectData -> ModelData
    getModelData staticObjectData = _modelData staticObjectData

    getTransformObjectData :: StaticObjectData -> TransformObjectData
    getTransformObjectData staticObjectData = _transformObject staticObjectData

    updateStaticObjectData :: StaticObjectData -> IO ()
    updateStaticObjectData staticObjectData = updateTransformObject (_transformObject staticObjectData)


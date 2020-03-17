{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}

module HulkanEngine3D.Render.Actor
    ( ActorData (..)
    , ActorInterface (..)
    ) where

import Data.IORef
import Data.Text
import Numeric.DataFrame
import HulkanEngine3D.Render.Model
import HulkanEngine3D.Render.TransformObject
import HulkanEngine3D.Utilities.System


data ActorData = ActorData
    { _name :: IORef Text
    , _modelData :: ModelData
    , _transformObject :: TransformObjectData
    } deriving (Show)


class ActorInterface a where
    newActorData :: Text -> ModelData -> IO a
    updateActorData :: a -> IO ()

instance ActorInterface ActorData where
    newActorData name modelData = do
        nameRef <- newIORef name
        transformObjectData <- newTransformObjectData
        setPosition transformObjectData (vec3 0 0 0)
        return ActorData
            { _name = nameRef
            , _modelData = modelData
            , _transformObject = transformObjectData
            }

    updateActorData actorData = return ()


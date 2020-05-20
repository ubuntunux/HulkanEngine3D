{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HulkanEngine3D.Render.Light where

import Control.Monad
import Data.IORef
import qualified Data.Text as T

import Numeric.DataFrame

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.TransformObject
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math


data LightCreateInfo = DirectionalLightCreateInfo
        { _directionalLightPosition' :: Vec3f
        , _directionalLightRotation' :: Vec3f
        , _directionalLightLightColor' :: Vec3f
        , _directionalLightShadowSamples' :: Int
        , _directionalLightShadowExp' :: Float
        , _directionalLightShadowBias' :: Float
        , _directionalLightShadowWidth' :: Float
        , _directionalLightShadowHeight' :: Float
        , _directionalLightShadowDepth' :: Float
        } deriving (Show)


getDefaultDirectionalLightCreateInfo :: LightCreateInfo
getDefaultDirectionalLightCreateInfo = DirectionalLightCreateInfo
    { _directionalLightPosition' = vec3 0 0 0
    , _directionalLightRotation' = vec3 0 -1 0
    , _directionalLightLightColor' = vec3 1 1 1
    , _directionalLightShadowSamples' = Constants.shadowSamples
    , _directionalLightShadowExp' = Constants.shadowExp
    , _directionalLightShadowBias' = Constants.shadowBias
    , _directionalLightShadowWidth' = Constants.shadowDistance
    , _directionalLightShadowHeight' = Constants.shadowDistance
    , _directionalLightShadowDepth' = Constants.shadowDistance
    }


data DirectionalLightData = DirectionalLightData
        { _directionalLightName :: IORef T.Text
        , _directionalLightLightColor :: IORef Vec3f
        , _directionalLightShadowSamples :: IORef Int
        , _directionalLightShadowExp :: IORef Float
        , _directionalLightShadowBias :: IORef Float
        , _directionalLightShadowWidth :: IORef Float
        , _directionalLightShadowHeight :: IORef Float
        , _directionalLightShadowDepth :: IORef Float
        , _directionalLightShadowProjection :: IORef Mat44f
        , _directionalLightShadowViewProjection :: IORef Mat44f
        , _directionalLightTransformObject :: TransformObjectData
        , _directionalLightDataChanged :: IORef Bool
        } deriving (Show)


class LightInterface a where
    createLightData :: T.Text -> LightCreateInfo -> IO a
    getShadowViewProjectionMatrix :: a -> IO Mat44f
    updateShadowOrthogonal :: a -> IO ()
    updateLightData :: a -> Vec3f -> IO ()

instance LightInterface DirectionalLightData where
    createLightData :: T.Text -> LightCreateInfo -> IO DirectionalLightData
    createLightData name directionalLightCreateData@DirectionalLightCreateInfo {..} = do
        logInfo $ "createLightData : " ++ T.unpack name
        directionalLightName <- newIORef name
        lightColor <- newIORef _directionalLightLightColor'
        shadowSamples <- newIORef _directionalLightShadowSamples'
        shadowExp <- newIORef _directionalLightShadowExp'
        shadowBias <- newIORef _directionalLightShadowBias'
        shadowWidth <- newIORef _directionalLightShadowWidth'
        shadowHeight <- newIORef _directionalLightShadowHeight'
        shadowDepth <- newIORef _directionalLightShadowDepth'
        shadowOrthogonal <- newIORef matrix4x4_indentity
        viewProjection <- newIORef matrix4x4_indentity
        transformObjectData <- newTransformObjectData
        dataChanged <- newIORef False
        let lightData = DirectionalLightData
                { _directionalLightName = directionalLightName
                , _directionalLightLightColor = lightColor
                , _directionalLightShadowSamples = shadowSamples
                , _directionalLightShadowExp = shadowExp
                , _directionalLightShadowBias = shadowBias
                , _directionalLightShadowWidth = shadowWidth
                , _directionalLightShadowHeight = shadowHeight
                , _directionalLightShadowDepth = shadowDepth
                , _directionalLightShadowProjection = shadowOrthogonal
                , _directionalLightShadowViewProjection = viewProjection
                , _directionalLightTransformObject = transformObjectData
                , _directionalLightDataChanged = dataChanged
                }
        setPosition transformObjectData _directionalLightPosition'
        setRotation transformObjectData _directionalLightRotation'
        updateLightData lightData float3_zero
        return lightData

    getShadowViewProjectionMatrix :: DirectionalLightData -> IO Mat44f
    getShadowViewProjectionMatrix lightData = readIORef $ _directionalLightShadowViewProjection lightData

    updateShadowOrthogonal :: DirectionalLightData -> IO ()
    updateShadowOrthogonal lightData@DirectionalLightData {..} = do
        width <- readIORef _directionalLightShadowWidth
        height <- readIORef _directionalLightShadowHeight
        depth <- readIORef _directionalLightShadowDepth
        let near = -depth
            far = depth
        writeIORef _directionalLightShadowProjection (orthogonal near far width height)
        writeIORef _directionalLightDataChanged True

    updateLightData :: DirectionalLightData -> Vec3f -> IO ()
    updateLightData lightData@DirectionalLightData {..} viewPosition = do
        updatedTransform <- updateTransformObject _directionalLightTransformObject
        dataChangedPrev <- readIORef _directionalLightDataChanged
        let dataChanged = dataChangedPrev || updatedTransform
            translationMatrix = translate3 (-viewPosition)
        when dataChanged $ do
            inverseMatrix <- getInverseMatrix _directionalLightTransformObject
            shadowProjection <- readIORef _directionalLightShadowProjection
            writeIORef _directionalLightShadowViewProjection (contract (contract translationMatrix inverseMatrix) shadowProjection)
        writeIORef _directionalLightDataChanged dataChanged
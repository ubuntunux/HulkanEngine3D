{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HulkanEngine3D.Utilities.Math where

import Numeric.DataFrame
import Numeric.Dimensions

-- | Get number of points in a vector
dataFrameLength :: DataFrame t (xns :: [XNat]) -> Word
dataFrameLength (XFrame (vector :: DataFrame t ns)) = case dims @ns of
    n :* _ -> dimVal n
    U      -> 1

float_zero :: Scalar Float
float_zero = S 0

float2_zero :: Vec2f
float2_zero = vec2 0 0

float3_zero :: Vec3f
float3_zero = vec3 0 0 0

float4_zero :: Vec4f
float4_zero = vec4 0 0 0 0

getFloat :: Float -> Scalar Float
getFloat v = S v

getFloat2 :: Float -> Vec2f
getFloat2 v = fromScalar (S v)

getFloat3 :: Float -> Vec3f
getFloat3 v = fromScalar (S v)

getFloat4 :: Float -> Vec4f
getFloat4 v = fromScalar (S v)

matrix4_indentity :: Mat44f
matrix4_indentity =
    DF4 (DF4 1 0 0 0)
        (DF4 0 1 0 0)
        (DF4 0 0 1 0)
        (DF4 0 0 0 1)
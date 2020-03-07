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
import Numeric.Quaternion


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

matrix4x4_indentity :: Mat44f
matrix4x4_indentity =
    DF4 (DF4 1 0 0 0)
        (DF4 0 1 0 0)
        (DF4 0 0 1 0)
        (DF4 0 0 0 1)

quaternion_identity :: Quater Float
quaternion_identity = Quater 0 0 0 1



world_left :: Vec3f
world_left = vec3 1 0 0

world_up :: Vec3f
world_up = vec3 0 1 0

world_front :: Vec3f
world_front = vec3 0 0 1

transform_matrix :: Vec3f -> Mat44f -> Vec3f -> Mat44f
transform_matrix translation rotation_matrix scale =
    let row0 = ((index (0:*U) rotation_matrix) :: Vec4f) * (fromScalar (index (0:*U) scale))
        row1 = ((index (1:*U) rotation_matrix) :: Vec4f) * (fromScalar (index (1:*U) scale))
        row2 = ((index (2:*U) rotation_matrix) :: Vec4f) * (fromScalar (index (2:*U) scale))
        row3 = ewmap (\(Vec3 x y z) -> vec4 x y z 1.0) translation
    in DF4 row0 row1 row2 row3
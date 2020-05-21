{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HulkanEngine3D.Utilities.Math where
import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Quaternion
import Data.Fixed

twoPI :: Floating a => a
twoPI = pi * 2.0

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

-- ... and a {clip space -> screen space} matrix that converts points into
--     the vulkan screen space {x: -1..1, y: 1..-1, z: 0..1}
clipSpaceMatrix :: Mat44f
clipSpaceMatrix = DF4
    (DF4 1   0   0   0)
    (DF4 0 (-1)  0   0)
    (DF4 0   0  0.5  0)
    (DF4 0   0  0.5  1)

quaternion_identity :: Quater Float
quaternion_identity = Quater 0 0 0 1

world_left :: Vec3f
world_left = vec3 1 0 0

world_up :: Vec3f
world_up = vec3 0 1 0

world_front :: Vec3f
world_front = vec3 0 0 1

wrap_rotation :: Float -> Float
wrap_rotation rotation
    | twoPI < rotation || rotation < 0.0 = mod' rotation twoPI
    | otherwise = rotation

matrixRotation :: Float -> Float -> Float -> Mat44f
matrixRotation rx ry rz =
    let ch = cos ry
        sh = sin ry
        ca = cos rz
        sa = sin rz
        cb = cos rx
        sb = sin rx
    in DF4 (vec4 (ch*ca) sa (-sh*ca) 0)
           (vec4 (sh*sb - ch*sa*cb) (ca*cb) (sh*sa*cb + ch*sb) 0)
           (vec4 (ch*sa*sb + sh*cb) (-ca*sb) (-sh*sa*sb + ch*cb) 0)
           (vec4 0 0 0 1)

transform_matrix :: Vec3f -> Mat44f -> Vec3f -> Mat44f
transform_matrix translation rotation_matrix scale =
    let (# sx, sy, sz #) = unpackV3# scale
        row0 = ((index (Idx 0:*U) rotation_matrix) :: Vec4f) * (getFloat4 sx)
        row1 = ((index (Idx 1:*U) rotation_matrix) :: Vec4f) * (getFloat4 sy)
        row2 = ((index (Idx 2:*U) rotation_matrix) :: Vec4f) * (getFloat4 sz)
        row3 = toHomPoint translation
    in DF4 row0 row1 row2 row3

inverse_transform_matrix :: Vec3f -> Mat44f -> Vec3f -> Mat44f
inverse_transform_matrix translation rotation_matrix scale =
    let rotation_matrix_T = transpose rotation_matrix
        (# sx, sy, sz #) = unpackV3# scale
        row0 = ((index (Idx 0:*U) rotation_matrix_T) :: Vec4f) / (getFloat4 sx)
        row1 = ((index (Idx 1:*U) rotation_matrix_T) :: Vec4f) / (getFloat4 sy)
        row2 = ((index (Idx 2:*U) rotation_matrix_T) :: Vec4f) / (getFloat4 sz)
        p = toHomVector (-translation)
        x = (index (Idx 0:*U) rotation_matrix) %* p
        y = (index (Idx 1:*U) rotation_matrix) %* p
        z = (index (Idx 2:*U) rotation_matrix) %* p
        row3 = DF4 x y z (scalar 1.0)
    in DF4 row0 row1 row2 row3
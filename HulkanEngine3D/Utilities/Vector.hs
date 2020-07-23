{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE UnboxedTuples          #-}

module HulkanEngine3D.Utilities.Vector where

import GHC.Generics
--import Foreign.C.Types
import Foreign
import Foreign.Storable ()

class VectorInterface a where
    norm :: a -> Scalar
    normalize :: a -> a


data Scalar = Scalar {-# UNPACK #-}!Float
                deriving (Eq, Show, Generic)

data Vec2 = Vec2 {-# UNPACK #-}!Float
                 {-# UNPACK #-}!Float
                 deriving (Eq, Show, Generic)

data Vec3 = Vec3 {-# UNPACK #-}!Float
                 {-# UNPACK #-}!Float
                 {-# UNPACK #-}!Float
                 deriving (Eq, Show, Generic)

data Vec4 = Vec4 {-# UNPACK #-}!Float
                 {-# UNPACK #-}!Float
                 {-# UNPACK #-}!Float
                 {-# UNPACK #-}!Float
                 deriving (Eq, Show, Generic)


instance Storable Scalar where
    sizeOf _ = sizeOf (undefined :: Float)
    alignment _ = alignment (undefined :: Float)

    {-# INLINE peek #-}
    peek v = do
        x <- peekByteOff (castPtr v) 0
        return (Scalar x)

    {-# INLINE poke #-}
    poke v (Scalar x) = do
        pokeByteOff (castPtr v) 0 x


instance Storable Vec2 where
    sizeOf _ = sizeOf (undefined :: Float) * 2
    alignment _ = alignment (undefined :: Float)

    {-# INLINE peek #-}
    peek v = do
        x <- peekByteOff vPtr 0
        y <- peekByteOff vPtr 4
        return (Vec2 x y)
        where
            vPtr = castPtr v

    {-# INLINE poke #-}
    poke v (Vec2 x y) = do
        pokeByteOff vPtr 0 x
        pokeByteOff vPtr 4 y
        where
            vPtr = castPtr v

instance Storable Vec3 where
    sizeOf _ = sizeOf (undefined :: Float) * 3
    alignment _ = alignment (undefined :: Float)

    {-# INLINE peek #-}
    peek v = do
        x <- peekByteOff vPtr 0
        y <- peekByteOff vPtr 4
        z <- peekByteOff vPtr 8
        return (Vec3 x y z)
        where
            vPtr = castPtr v

    {-# INLINE poke #-}
    poke v (Vec3 x y z) = do
        pokeByteOff vPtr 0 x
        pokeByteOff vPtr 4 y
        pokeByteOff vPtr 8 z
        where
            vPtr = castPtr v

instance Storable Vec4 where
    sizeOf _ = sizeOf (undefined :: Float) * 4
    alignment _ = alignment (undefined :: Float)

    {-# INLINE peek #-}
    peek v = do
        x <- peekByteOff vPtr 0
        y <- peekByteOff vPtr 4
        z <- peekByteOff vPtr 8
        w <- peekByteOff vPtr 12
        return (Vec4 x y z w)
        where
            vPtr = castPtr v

    {-# INLINE poke #-}
    poke v (Vec4 x y z w) = do
        pokeByteOff vPtr 0 x
        pokeByteOff vPtr 4 y
        pokeByteOff vPtr 8 z
        pokeByteOff vPtr 12 w
        where
            vPtr = castPtr v

instance VectorInterface Vec3 where
    norm (Vec3 x0 y0 z0) = Scalar $ sqrt (x0 * x0 + y0 * y0 + z0 * z0)
    normalize vec@(Vec3 x0 y0 z0)
            | length /= 0.0 = Vec3 (x0 / length) (y0 / length) (z0 / length)
            | otherwise = vec
            where
                (Scalar length) = norm vec

instance Num Vec3 where
    (Vec3 x0 y0 z0) + (Vec3 x1 y1 z1) = Vec3 (x0 + x1) (y0 + y1) (z0 + z1)
    (Vec3 x0 y0 z0) - (Vec3 x1 y1 z1) = Vec3 (x0 - x1) (y0 - y1) (z0 - z1)
    (Vec3 x0 y0 z0) * (Vec3 x1 y1 z1) = Vec3 (x0 * x1) (y0 * y1) (z0 * z1)
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
    fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

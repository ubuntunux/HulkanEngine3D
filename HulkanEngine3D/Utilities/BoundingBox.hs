{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE NegativeLiterals       #-}

module HulkanEngine3D.Utilities.BoundingBox where

import GHC.Generics (Generic)

import Numeric.DataFrame

data BoundingBox = BoundingBox
    { _boundingBoxMin :: Vec3f
    , _boundingBoxMax :: Vec3f
    , _boundingBoxCenter :: Vec3f
    , _boundingBoxRadius :: Float
    } deriving (Eq, Show, Generic)

defaultBoundingBox ::  BoundingBox
defaultBoundingBox =
    let minValue = vec3 -1 -1 -1
        maxValue = vec3 1 1 1
        S radius = normL2 $ (maxValue - minValue)
    in BoundingBox
        { _boundingBoxMin = minValue
        , _boundingBoxMax = maxValue
        , _boundingBoxCenter = (minValue + maxValue) * 0.5
        , _boundingBoxRadius = radius
        }

calcBoundingBox :: [Vec3f] -> BoundingBox
calcBoundingBox [] = defaultBoundingBox
calcBoundingBox (position:positions) =
    let (minValue, maxValue) = calcBoundingBox' position position positions
        S radius = normL2 $ (maxValue - minValue)
    in BoundingBox
           { _boundingBoxMin = minValue
           , _boundingBoxMax = maxValue
           , _boundingBoxCenter = (minValue + maxValue) * 0.5
           , _boundingBoxRadius = radius
           }
    where
        calcBoundingBox' :: Vec3f -> Vec3f -> [Vec3f] -> (Vec3f, Vec3f)
        calcBoundingBox' boundMin boundMax [] = (boundMin, boundMax)
        calcBoundingBox' boundMin boundMax (position:positions) =
            let (# minX, minY, minZ #) = unpackV3# boundMin
                (# maxX, maxY, maxZ #) = unpackV3# boundMax
                minValue = vec3 (min minX maxX) (min minY maxY) (min minZ maxZ)
                maxValue = vec3 (max minX maxX) (max minY maxY) (max minZ maxZ)
            in
                calcBoundingBox' minValue maxValue positions

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UnboxedTuples          #-}

module HulkanEngine3D.Utilities.DataFrame where

import qualified Data.Text as Text ()

import Data.Aeson
import Numeric.DataFrame


instance ToJSON Scf where
    toJSON (S x) = object [ "S" .= x ]
    toEncoding (S x) = pairs ( "S" .= x )

instance FromJSON Scf where
    parseJSON (Object v) = do
            x <- v .: "S"
            return (S x)
    parseJSON _ = error ""


instance ToJSON Vec2f where
    toJSON (Vec2 x y) = object [ "Vec2" .= (x, y) ]
    toEncoding (Vec2 x y) = pairs ( "Vec2" .= (x, y) )

instance FromJSON Vec2f where
    parseJSON (Object v) = do
            (x, y) <- v .: "Vec2"
            return (Vec2 x y)
    parseJSON _ = error ""


instance ToJSON Vec3f where
    toJSON (Vec3 x y z) = object [ "Vec3" .= (x, y, z) ]
    toEncoding (Vec3 x y z) = pairs ( "Vec3" .= (x, y, z) )

instance FromJSON Vec3f where
    parseJSON (Object v) = do
            (x, y, z) <- v .: "Vec3"
            return (Vec3 x y z)
    parseJSON _ = error ""
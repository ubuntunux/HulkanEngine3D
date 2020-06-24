{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
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

import Data.Aeson
import Numeric.DataFrame
import Graphics.Vulkan.Core_1_0

instance ToJSON Scf
instance FromJSON Scf

instance ToJSON (Scalar Word32)
instance FromJSON (Scalar Word32)

instance ToJSON Vec2f
instance FromJSON Vec2f

instance ToJSON Vec3f
instance FromJSON Vec3f
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Library.Utilities.Math
  ( dataFrameLength
  ) where

import Numeric.DataFrame
import Numeric.Dimensions

-- | Get number of points in a vector
dataFrameLength :: DataFrame t (xns :: [XNat]) -> Word
dataFrameLength (XFrame (_ :: DataFrame t ns)) = case dims @ns of
    n :* _ -> dimVal n
    U      -> 1
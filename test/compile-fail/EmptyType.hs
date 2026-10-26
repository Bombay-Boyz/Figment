{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
-- Must fail: an empty data type cannot be decoded.
module EmptyType where

import GHC.Generics (Generic)
import Figment (FromConfig)

data Never
  deriving stock (Generic)
  deriving anyclass (FromConfig)

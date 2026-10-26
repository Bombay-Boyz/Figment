{-# LANGUAGE DeriveGeneric, DerivingStrategies, TypeFamilies #-}
-- Must fail: a Partial configuration must be a single-constructor record.
module PartialSum where

import Data.Text (Text)
import GHC.Generics (Generic)
import Figment (Field, Partial)

data ModeF f = A {a :: Field f Text} | B {b :: Field f Int}
  deriving stock (Generic)

instance Partial ModeF

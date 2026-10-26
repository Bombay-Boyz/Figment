{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, OverloadedStrings, TypeFamilies #-}
-- Must compile: the positive control for the fixtures above.
module Ok where

import Data.Text (Text)
import GHC.Generics (Generic)
import Figment (Field, FromConfig, Partial, Figment, new, merge, prefixed)

data AppF f = App {host :: Field f Text, port :: Field f Int}
  deriving stock (Generic)

instance Partial AppF

data Plain = Plain {x :: Int}
  deriving stock (Generic)
  deriving anyclass (FromConfig)

figment :: Figment
figment = merge (prefixed "APP_") new

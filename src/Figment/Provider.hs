{-# LANGUAGE Safe #-}
module Figment.Provider
  ( Provider(..)
  , tagProviderDefault
  , EnvPrefixed(..)
  , CliArgs(..)
  , Defaults(..)
  , FieldProvider(..)
  ) where

import Data.Text (Text)

import Figment.Error (ConfigError)
import Figment.Value (Depth, Tagged, defaultDepth)

class Provider p where
  tagProvider :: Depth -> p -> IO (Either ConfigError Tagged)

tagProviderDefault :: Provider p => p -> IO (Either ConfigError Tagged)
tagProviderDefault = tagProvider defaultDepth

newtype EnvPrefixed     = EnvPrefixed Text
newtype CliArgs         = CliArgs [Text]
newtype Defaults r      = Defaults r
newtype FieldProvider p = FieldProvider p

instance Provider p => Provider (FieldProvider p) where
  tagProvider d (FieldProvider p) = tagProvider d p

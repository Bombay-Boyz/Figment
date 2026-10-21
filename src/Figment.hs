{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Figment
  ( Key(..)
  , SourceId(..)
  , Path(..)
  , Profile(..)
  , ProfileName(..)
  , Depth
  , mkDepth
  , defaultDepth
  , Tagged(..)
  , Value(..)
  , Metadata(..)
  , ConfigError(..)
  , Layer(..)
  , runLayers
  , runExtract
  , Provider(..)
  , tagProviderDefault
  , EnvPrefixed(..)
  , CliArgs(..)
  , Defaults(..)
  , FieldProvider(..)
  , JsonFile(..)
  , TomlFile(..)
  , YamlFile(..)
  , composeChecked
  , composeUnchecked
  ) where

import Data.Aeson (FromJSON)
import Figment.Check (RequireGenericProvider)
import Figment.Error (ConfigError(..))
import Figment.Extract (runExtract)
import Figment.Layer (Layer(..), runLayers)
import Figment.Provider
  ( Provider(..), tagProviderDefault
  , EnvPrefixed(..), CliArgs(..), Defaults(..), FieldProvider(..) )
import Figment.Provider.Json (JsonFile(..))
import Figment.Provider.Toml (TomlFile(..))
import Figment.Provider.Yaml (YamlFile(..))
import Figment.Value
  ( Key(..), SourceId(..), Path(..), Profile(..), ProfileName(..)
  , Depth, mkDepth, defaultDepth, Tagged(..), Value(..), Metadata(..) )

-- | Compose layers into a record, with a compile-time check that at
-- least one provider in the chain can supply non-file values.
-- -Wredundant-constraints is disabled for this module because
-- 'RequireGenericProvider' is a type-level-only gate whose evidence is
-- discharged entirely at compile time (ADR 007).
composeChecked
  :: forall r ps
   . (RequireGenericProvider r ps, FromJSON r)
  => [Layer] -> Either ConfigError r
composeChecked = runExtract . runLayers

composeUnchecked :: FromJSON r => [Layer] -> Either ConfigError r
composeUnchecked = runExtract . runLayers

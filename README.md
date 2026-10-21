# Figment for Haskell

A type-safe, depth-bounded configuration merging library for Haskell featuring provenance tracking and compile-time provider verification. Inspired by Rust's `figment` crate.

---

## Overview

**Figment** enables application developers to merge settings from multiple layered sources—such as default code values, TOML, JSON, YAML, Dhall files, environment variables, and command-line arguments—into a single, validated Haskell record. 

Every step in Figment's pipeline is backed by explicit algebraic rules:
* **Provenance guarantees**: Every leaf node in a parsed configuration tree tracks its original source, active profile, and exact root-to-leaf key path.
* **Bounded execution**: Hostile or deeply nested input formats fail gracefully via explicit stack budgets rather than crashing your runtime with a stack overflow.
* **Compile-time verification**: Type families prevent incomplete provider pipelines at compile time before your program ever runs.

---

## Features

* **Provenance Tracking**: Every configuration value carries metadata detailing its original source ID, profile name, and exact structural key path.
* **Stack & Resource Safety**: Every provider enforces a caller-controllable recursion budget (`Depth`, defaulting to 64) to neutralize stack-exhaustion attacks on untrusted inputs.
* **Cross-Strategy Layer Merging**: Explicit control over left- vs. right-bias and array replacement vs. concatenation across layered configuration inputs.
* **Compile-Time Reachability**: Catches missing provider errors at compile time using type-level verification (`composeChecked`).
* **Closed Error Vocabulary**: Captures failure modes across all ingestion stages inside a single, structured `ConfigError` type.

---

## Architecture

Figment follows a hexagonal architecture separating core AST, merge algebra, type-level checks, and adapter providers:

```text
figment/
  src/
    Figment/
      Value.hs         -- Value & Tagged domain models; provenance tracking
      Merge.hs         -- Core merge operators (Bias, ArrayPolicy, mergeAlg)
      Layer.hs         -- Ordered layer composition and execution (runLayers)
      Error.hs         -- Stage-specific closed ConfigError type
      Check.hs         -- Type families for compile-time provider reachability
      Provider.hs      -- Core Provider class & built-in generic providers
      Provider/        -- Standard format adapters (Env, Cli, Toml, Json, Yaml, Dhall)
      Figment.hs       -- Public API façade
      
Merge AlgebraFigment combines configuration trees using four explicit strategies defined by Bias (LeftWins vs. RightWins) and Array Policy (Replace vs. Concat):   StrategyBiasArray PolicyBehaviormergeRightWinsReplaceRight operand overwrites left; arrays are replaced.   joinLeftWinsReplaceLeft operand takes precedence; arrays are replaced.   admergeRightWinsConcatRight operand overwrites left; array elements are appended[cite: 1].adjoinLeftWinsConcatLeft operand takes precedence; array elements are appended[cite: 1].Because mixed-strategy merge chains are non-associative, Figment evaluates layer chains as an explicit, strict left-fold (runLayers) and deliberately omits Semigroup or Monoid instances[cite: 1].Quickstart1. Define Your Configuration RecordHaskell{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Figment
import Figment.Error (ConfigError(..))

-- Raw intermediate record matching incoming provider key shapes
data AppConfigRaw = AppConfigRaw
  { rHost     :: Text
  , rPort     :: Int
  , rLogLevel :: Text
  , rDatabase :: Text
  } deriving (Generic, FromJSON)

-- Validated runtime configuration
data AppConfig = AppConfig Text Port Text Text

newtype Port = UnsafePort Int deriving (Eq, Show)

mkPort :: Int -> Either ConfigError Port
mkPort n
  | n >= 1 && n <= 65535 = Right (UnsafePort n)
  | otherwise = Left (ValidationError (Path []) "port must be in 1..65535")
2. Configure Providers & Load ConfigurationHaskellimport Figment.Provider (EnvPrefixed(..), Defaults(..))
import Figment.Provider.Toml (TomlFile(..))

-- Source configuration layers from defaults, TOML files, and environment variables
configSource :: Depth -> [IO (Either ConfigError Layer)]
configSource d =
  [ fmap Join  <$> tagProvider d (Defaults defaultConfigRaw)
  , fmap Merge <$> tagProvider d (TomlFile "config.toml")
  , fmap Merge <$> tagProvider d (EnvPrefixed "APP_")
  ]

loadConfig :: IO (Either ConfigError AppConfig)
loadConfig = do
  -- Resolve layers with default recursion depth (64)
  rawLayers <- sequence (configSource defaultDepth)
  pure $ do
    layers <- sequence rawLayers
    -- Extract and validate raw configuration
    raw <- runExtract (runLayers layers) :: Either ConfigError AppConfigRaw
    AppConfig (rHost raw) 
      <$> mkPort (rPort raw)
      <*> pure (rLogLevel raw) 
      <*> pure (rDatabase raw)
Compile-Time Provider ChecksTo ensure that required configuration keys with no file-based fallback can be supplied at runtime, use composeChecked[cite: 1]. If a provider list lacks generic fallbacks (such as EnvPrefixed, CliArgs, or Defaults), GHC will reject the program at compile time[cite: 1]:Haskell-- Fails to compile if TomlFile is the only provider and non-file values are needed:
cfg :: Either ConfigError AppConfig
cfg = composeChecked @AppConfig @'[TomlFile, EnvPrefixed] layers
For intentionally file-only configurations, use composeUnchecked to bypass this compile-time constraint[cite: 1].Error HandlingAll pipeline failures are unified into the closed ConfigError sum type[cite: 1]:SourceError FilePath Text: Provider I/O or parsing failure[cite: 1].DepthExceeded Path Depth: Input nesting exceeded caller recursion budget[cite: 1].DecodeError Path Text: Shape or type mismatch during record extraction[cite: 1].MissingKey Path: Required configuration key missing across all layers[cite: 1].ValidationError Path Text: Smart constructor rule violation during boundary validation[cite: 1].ConfigurationError Text Text: Library invariant or precondition violation[cite: 1].

-- | Layered configuration with provenance: a Haskell take on Rust's
-- figment. New here? Read "Figment.Tutorial" first.
--
-- > import Data.Function ((&))
-- > import qualified Figment as F
-- > import qualified Figment.Provider.Env as Env
-- >
-- > data Config = Config {name :: Text, port :: Int, debug :: Bool}
-- >   deriving stock (Show, Generic)
-- >   deriving anyclass (F.FromConfig)
-- >
-- > main :: IO ()
-- > main = do
-- >   cfg <- F.loadExtractOrThrow @Config $
-- >     F.new
-- >       & F.merge (F.file F.toml "App.toml")
-- >       & F.merge (F.prefixed "APP_" & Env.split "__")
-- >       & F.merge (F.cli ["--port=9000"])
-- >   print cfg
--
-- Every value remembers its source, and failures are reported all at
-- once, each with its key path, the provider-native name (@APP_PORT@,
-- @--port@), the file, and the line and column.
--
-- This module re-exports the whole API. Import it qualified, because it
-- exports 'join', 'merge' and 'from'. Options for individual providers
-- live in "Figment.Provider.Env", "Figment.Provider.Cli" and
-- "Figment.Provider.Format". Test helpers are in "Figment.Jail".
module Figment
  ( -- * Building a figment
    Figment
  , new
  , from
  , merge
  , join
  , adjoin
  , admerge
  , combine
  , Order (..)
  , select
  , withLimits
  , figmentSources
  , Provider (..)
    -- * Loading
  , Loaded
  , load
  , loadedProfile
  , loadedSources
  , profiles
  , selected
  , reselect
  , findValue
  , contains
  , findMetadata
  , focus
    -- * Extracting
  , extract
  , extractLossy
  , extractAt
  , extractWith
  , loadExtract
  , loadExtractOrThrow
    -- * Partial (higher-kinded) configurations
  , Field
  , Partial (..)
  , extractPartial
  , extractPartialWith
  , extractTotal
  , extractTotalWith
  , Identity (..)
    -- * Decoding
  , FromConfig (..)
  , FromConfigKey (..)
  , Decoder
  , Result (..)
  , DecodeContext (..)
  , runDecoder
  , decodeWith
  , mkDecoder
  , andThen
  , withValue
  , currentPath
  , failWith
  , validate
  , field
  , fieldWith
  , optionalField
  , fieldOr
  , arrayOf
  , dictOf
  , textual
  , boundedIntegral
  , Options (..)
  , defaultOptions
  , snakeCase
  , kebabCase
  , genericDecoder
  , Tagged (..)
  , taggedMetadata
  , RelativePath (..)
  , relative
  , ViaAeson (..)
  , aesonDecoder
    -- * Providers
    -- ** Files and strings
  , Format
  , Data
  , json
  , toml
  , yaml
  , file
  , fileExact
  , fromText
  , nested
  , required
    -- ** Environment variables (see "Figment.Provider.Env" for options)
  , Env
  , prefixed
    -- ** Command-line arguments (see "Figment.Provider.Cli")
  , Cli
  , cli
  , cliFromProcess
    -- ** Haskell values
  , Serialized
  , defaults
  , globals
  , keyed
    -- * Values
  , Value (..)
  , Node (..)
  , Number (..)
  , Dict
  , Tag (..)
    -- * Paths
  , Path
  , Segment (..)
  , root
  , keyPath
  , parsePath
  , renderPath
  , (/.)
  , (/#)
    -- * Profiles
  , Profile (Default, Global)
  , profile
  , profileName
  , profileFromEnv
    -- * Metadata
  , Metadata (..)
  , Source (..)
  , Pos (..)
  , named
    -- * Errors
  , ConfigError (..)
  , ConfigErrors
  , ErrorKind (..)
  , errorList
  , renderError
  , renderErrors
    -- * Limits
  , Limits
  , mkLimits
  , withMaxBytes
  , defaultLimits
  , defaultMaxBytes
  , maxDepth
  , maxNodes
  , maxBytes
  ) where

import Data.Functor.Identity (Identity (..))

import Figment.Aeson
import Figment.Core
import Figment.Decode
import Figment.Error
import Figment.Extract
import Figment.Limits
import Figment.Merge
import Figment.Metadata
import Figment.Partial
import Figment.Path
import Figment.Profile
import Figment.Provider.Cli
import Figment.Provider.Env (Env, prefixed)
import Figment.Provider.Format
import Figment.Provider.Json
import Figment.Provider.Toml
import Figment.Provider.Yaml
import Figment.Value

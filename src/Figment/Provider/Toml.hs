{-# OPTIONS_GHC -Wno-orphans #-}
-- | TOML file provider (§8.2 of the feasibility study).
module Figment.Provider.Toml
  ( TomlFile(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Toml
import Toml (Value'(..), Table'(..))

import Figment.Error (ConfigError(..))
import Figment.Provider (Provider(..))
import Figment.Value
  ( SourceId(..), Profile(..), RawJSON(..), mkTagged )

newtype TomlFile = TomlFile FilePath

instance Provider TomlFile where
  tagProvider d (TomlFile path) = do
    text <- TIO.readFile path
    pure $ case Toml.parse text of
      Left err  -> Left (SourceError path (Text.pack err))
      Right ast -> mkTagged d (SourceId (Text.pack ("toml:" ++ path)), DefaultProfile)
                              (fromTomlTable ast)

fromTomlTable :: Table' a -> RawJSON
fromTomlTable (MkTable t) =
  RawObj [ (k, fromTomlValue v) | (k, (_ann, v)) <- Map.toList t ]

fromTomlValue :: Value' a -> RawJSON
fromTomlValue v = case v of
  Table'     _ t -> fromTomlTable t
  List'      _ a -> RawArr (map fromTomlValue a)
  Text'      _ s -> RawStr s
  Integer'   _ n -> RawNum (fromIntegral n)
  Double'    _ n -> RawNum (realToFrac n)
  Bool'      _ b -> RawBool b
  _              -> RawNull

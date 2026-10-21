{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | TOML file provider (§8.2 of the feasibility study).
module Figment.Provider.Toml
  ( TomlFile(..)
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Toml
import Toml (Value'(..), Table'(..))

import Figment.Error (ConfigError(..))
import Figment.Provider (Provider(..))
import Figment.Value
  ( SourceId(..), Profile(..), RawJSON(..), mkTagged )

newtype TomlFile = TomlFile FilePath

instance Provider TomlFile where
  tagProvider d (TomlFile path) = do
    outcome <- try $ do
      bytes <- BS.readFile path
      let text = TE.decodeUtf8With (\_ _ -> Just '\xfffd') bytes
      pure (Toml.parse text)
    pure $ case outcome :: Either SomeException (Either String (Table' Toml.Position)) of
      Left e            -> Left (SourceError path (Text.pack (show e)))
      Right (Left err)  -> Left (SourceError path (Text.pack err))
      Right (Right ast) ->
        mkTagged d (SourceId (Text.pack ("toml:" ++ path)), DefaultProfile)
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

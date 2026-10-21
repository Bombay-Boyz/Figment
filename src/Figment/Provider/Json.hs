-- | JSON file provider (§8.5 of the feasibility study). The
-- aeson-to-RawJSON boundary is defined once, here.
module Figment.Provider.Json
  ( JsonFile(..)
  , fromAeson
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Figment.Error (ConfigError(..))
import Figment.Provider (Provider(..))
import Figment.Value (SourceId(..), Profile(..), RawJSON(..), mkTagged)

newtype JsonFile = JsonFile FilePath

instance Provider JsonFile where
  tagProvider d (JsonFile path) = do
    bytes <- BL.readFile path
    pure $ case Aeson.eitherDecode bytes of
      Left err -> Left (SourceError path (Text.pack err))
      Right v  -> mkTagged d (SourceId (Text.pack ("json:" ++ path)), DefaultProfile)
                          (fromAeson v)

fromAeson :: Aeson.Value -> RawJSON
fromAeson Aeson.Null       = RawNull
fromAeson (Aeson.Bool   b) = RawBool b
fromAeson (Aeson.Number n) = RawNum  n
fromAeson (Aeson.String t) = RawStr  t
fromAeson (Aeson.Array  a) = RawArr (map fromAeson (Vector.toList a))
fromAeson (Aeson.Object o) =
  RawObj [ (Aeson.Key.toText k, fromAeson v)
         | (k, v) <- Aeson.KeyMap.toList o ]

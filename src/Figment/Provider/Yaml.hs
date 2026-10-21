{-# OPTIONS_GHC -Wno-orphans #-}
-- | YAML file provider (§8.2 of the feasibility study). Parses via
-- 'yaml' (HsYAML-compatible) and flattens through 'fromAeson'.
module Figment.Provider.Yaml
  ( YamlFile(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.Yaml as Yaml

import Figment.Error (ConfigError(..))
import Figment.Provider (Provider(..))
import Figment.Provider.Json (fromAeson)
import Figment.Value
  ( SourceId(..), Profile(..), mkTagged )

newtype YamlFile = YamlFile FilePath

instance Provider YamlFile where
  tagProvider d (YamlFile path) = do
    bytes <- BS.readFile path
    pure $ case Yaml.decodeEither' bytes of
      Left err  -> Left (SourceError path (Text.pack (Yaml.prettyPrintParseException err)))
      Right val -> mkTagged d (SourceId (Text.pack ("yaml:" ++ path)), DefaultProfile)
                              (fromAeson (val :: Aeson.Value))

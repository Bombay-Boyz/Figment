-- | Where did each value come from?
--
-- 'sources' lists every leaf of the selected profile together with the
-- provider, source file, position and provider-native key (for example
-- @APP_DB__PORT@) that produced the winning value. It answers the first
-- question anyone asks of layered configuration: /why does this key have
-- this value?/
--
-- Values themselves are never included, so a report is safe to print or
-- attach to a ticket even when the configuration holds secrets.
module Figment.Report
  ( Entry (..)
  , sources
  , renderSources
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Figment.Core
import Figment.Internal.Text (escapeControl)
import Figment.Metadata
import Figment.Path
import Figment.Profile
import Figment.Value

-- | The provenance of one value.
data Entry = Entry
  { entryPath :: !Path
    -- ^ where the value sits in the configuration
  , entryKind :: !Text
    -- ^ @null@, @boolean@, @number@, @string@, @array@ or @dictionary@
    -- (the last two only when empty: otherwise their contents are listed)
  , entryProvider :: !(Maybe Text)
    -- ^ the provider's name, e.g. @TOML file@
  , entrySource :: !(Maybe Source)
    -- ^ the file or code location, when the provider has one
  , entryPos :: !(Maybe Pos)
    -- ^ line and column in that source, when known
  , entryNative :: !(Maybe Text)
    -- ^ the key as that provider spells it, when it differs from the path
    -- (an environment variable name, a command-line flag)
  , entryProfile :: !(Maybe Profile)
    -- ^ the profile the value came from
  }
  deriving stock (Eq, Show)

-- | Every leaf value of the selected profile, in key order.
sources :: Loaded -> [Entry]
sources = go root . selected
  where
    go :: Path -> Value -> [Entry]
    go p v = case valueNode v of
      Dict d
        | not (Map.null d) -> concat [go (p /. k) x | (k, x) <- Map.toAscList d]
      Array xs
        | not (Vector.null xs) -> concat [go (p /# i) x | (i, x) <- zip [0 ..] (Vector.toList xs)]
      n -> [entry p (kind n) (valueTag v)]
    kind = \case
      Null -> "null"
      Bool _ -> "boolean"
      Num _ -> "number"
      String _ -> "string"
      Array _ -> "array"
      Dict _ -> "dictionary"
    entry p k t =
      Entry
        { entryPath = p
        , entryKind = k
        , entryProvider = metaName <$> tagMeta t
        , entrySource = tagMeta t >>= metaSource
        , entryPos = tagPos t
        , entryNative = do
            m <- tagMeta t
            let n = interpolate m (maybe Default id (tagProfile t)) p
            if n == renderPath p then Nothing else Just n
        , entryProfile = tagProfile t
        }

-- | One line per value:
--
-- > db.port: number from environment variables (prefix APP_) as `APP_DB__PORT`
-- > db.host: string from TOML file /etc/app.toml:3:8
--
-- Everything that came from a configuration source is escaped, so a hostile
-- key or file name cannot forge extra lines.
renderSources :: Loaded -> Text
renderSources l = Text.unlines (map line (sources l))
  where
    line e =
      escapeControl (renderPath (entryPath e))
        <> ": "
        <> entryKind e
        <> fromP e
        <> maybe "" (\n -> " as `" <> escapeControl n <> "`") (entryNative e)
        <> case entryProfile e of
          Just p | p /= Default -> " [profile `" <> escapeControl (profileName p) <> "`]"
          _ -> ""
    fromP e = case entryProvider e of
      Nothing -> ""
      Just name -> " from " <> escapeControl name <> where_ e
    where_ e = case entrySource e of
      Nothing -> maybe "" (\(Pos a b) -> " at line " <> tshow a <> ", column " <> tshow b) (entryPos e)
      Just s -> " " <> escapeControl (renderSource s) <> case entryPos e of
        Just (Pos a b) | isFile s -> ":" <> tshow a <> ":" <> tshow b
        _ -> ""
    isFile = \case
      FileSource _ -> True
      _ -> False
    tshow :: Show a => a -> Text
    tshow = Text.pack . show

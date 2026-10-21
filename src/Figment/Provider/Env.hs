{-# OPTIONS_GHC -Wno-orphans #-}
-- | Environment-variable provider (§8.2 of the feasibility study).
-- The 'Provider' instance for 'EnvPrefixed' lives here rather than in
-- 'Figment.Provider' because that is where the adapter logic lives
-- (feasibility study §9); see ADR 009.
module Figment.Provider.Env
  ( EnvPrefixed(..)
  , parseEnvKey
  ) where

import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getEnvironment)

import Figment.Provider (Provider(..), EnvPrefixed(..))
import Figment.Value
  ( SourceId(..), Profile(..), RawJSON(..), mkTagged )

parseEnvKey :: Text -> Text -> Maybe [Text]
parseEnvKey prefix name =
  case Text.stripPrefix prefix name of
    Nothing        -> Nothing
    Just remainder ->
      let parts = filter (not . Text.null)
                    (Text.splitOn (Text.pack "_")
                       (Text.map toLower remainder))
      in if null parts then Nothing else Just parts

instance Provider EnvPrefixed where
  tagProvider d (EnvPrefixed prefix) = do
    env <- getEnvironment
    let relevant =
          [ (ks, Text.pack v)
          | (k, v) <- env
          , Just ks <- [parseEnvKey prefix (Text.pack k)] ]
        raw = buildRaw relevant
    pure (mkTagged d (SourceId (Text.append "env:" prefix), DefaultProfile) raw)

buildRaw :: [([Text], Text)] -> RawJSON
buildRaw entries =
  foldr (\(ks, v) acc -> insert ks v acc) (RawObj []) deduped
  where
    deduped :: [([Text], Text)]
    deduped = Map.toList (Map.fromList entries)

    insert :: [Text] -> Text -> RawJSON -> RawJSON
    insert [] _ acc = acc
    insert (k:ks) v (RawObj fs) =
      let sub = lookup k fs
          sub' = case sub of
                   Just existing -> insert ks v existing
                   Nothing       -> insert ks v (RawObj [])
      in RawObj ((k, sub') : filter ((/= k) . fst) fs)
    insert _ _ acc = acc

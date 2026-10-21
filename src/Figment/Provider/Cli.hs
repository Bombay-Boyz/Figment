{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | CLI-argument provider (§8.2 of the feasibility study). Arguments of
-- the form @--key=value@ or @--key value@ become leaves; keys are
-- lowercased and dots split path components. See ADR 010 for the rule.
module Figment.Provider.Cli
  ( CliArgs(..)
  , parseCliArg
  ) where

import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Figment.Provider (Provider(..), CliArgs(..))
import Figment.Value
  ( SourceId(..), Profile(..), RawJSON(..), mkTagged )

-- | Parse one CLI argument of the form @--key=value@ or the pair
-- @("--key", "value")@. Dots in @key@ split path components. Returns
-- 'Nothing' if the argument is not a long option.
parseCliArg :: Text -> Maybe (Text, Text)
parseCliArg arg = do
  rest  <- Text.stripPrefix (Text.pack "--") arg
  let (k, withEq) = Text.breakOn (Text.pack "=") rest
  if Text.null k then Nothing else do
    v <- Text.stripPrefix (Text.pack "=") withEq
    pure (Text.map toLower k, v)

instance Provider CliArgs where
  tagProvider d (CliArgs args) =
    let pairs   = [ (splitKey k, v) | arg <- args, Just (k, v) <- [parseCliArg arg] ]
        raw     = buildRaw pairs
    in pure (mkTagged d (SourceId (Text.pack "cli"), DefaultProfile) raw)
    where
      splitKey :: Text -> [Text]
      splitKey = Text.splitOn (Text.pack ".")

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

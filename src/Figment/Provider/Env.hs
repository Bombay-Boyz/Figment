-- | Environment variables, with Rust figment's semantics.
--
-- > import qualified Figment.Provider.Env as Env
-- > Env.prefixed "APP_"                     -- APP_MAX_CONNECTIONS -> max_connections
-- > Env.prefixed "APP_" & Env.split "__"    -- APP_DB__HOST -> db.host
--
-- * The prefix is matched case-insensitively and stripped.
-- * Keys are lower-cased (see 'lowercase').
-- * There is no splitting unless 'split' is used, so fields whose names
--   contain underscores remain reachable. As in Rust figment, a @.@ in a
--   key (after any 'split') nests, and variables with an empty key
--   segment are ignored.
-- * Values are parsed: @8080@ is a number, @true@ a boolean, @[1, 2]@
--   an array, @{a = 1}@ a dictionary; anything else is a string. The
--   original text is kept, so 'Text' fields still receive @007@
--   verbatim. Use 'unparsed' to keep every value a string.
-- * Two variables that cannot both hold (@APP_DB=x@ and @APP_DB.HOST=y@,
--   or two names that are equal after lower-casing) are reported as
--   errors rather than one silently winning.
module Figment.Provider.Env
  ( Env
  , prefixed
  , raw
  , split
  , lowercase
  , ignoreEmpty
  , only
  , ignore
  , filterKeys
  , mapKeys
  , inProfile
  , global
  , unparsed
  , fromList
  , envVariableName
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getEnvironment)

import Figment.Core
import Figment.Error
import Figment.Internal.Nest
import Figment.Internal.ValueParser (parseValue)
import Figment.Metadata
import Figment.Path
import Figment.Profile
import Figment.Value

-- | Environment variables as a configuration source; build with 'prefixed' or 'raw'.
data Env = Env
  { envPrefix :: !Text
  , envChain :: [Text -> Maybe Text]
    -- ^ key transformations, applied in the order they were added
  , envSplit :: !(Maybe Text)
  , envLower :: !Bool
  , envIgnoreEmpty :: !Bool
  , envProfile :: !Profile
  , envParsed :: !Bool
  , envSnapshot :: !(Maybe [(Text, Text)])
  }

-- | Variables starting with the prefix (case-insensitive), prefix
-- removed.
prefixed :: Text -> Env
prefixed p = Env p [] Nothing True False Default True Nothing

-- | Every environment variable.
raw :: Env
raw = prefixed ""

-- | Nest on a separator: with @split \"\__\"@, @APP_DB\__HOST@ is @db.host@.
split :: Text -> Env -> Env
split sep e
  | Text.null sep = e
  | otherwise = e {envSplit = Just sep, envChain = envChain e ++ [Just . Text.replace sep "."]}

-- | Lower-case keys (default 'True').
lowercase :: Bool -> Env -> Env
lowercase b e = e {envLower = b}

-- | Skip variables whose value is empty (default 'False').
ignoreEmpty :: Bool -> Env -> Env
ignoreEmpty b e = e {envIgnoreEmpty = b}

-- | Keep only these keys (compared case-insensitively, after the prefix
-- and any earlier transformations).
only :: [Text] -> Env -> Env
only ks = filterKeys (\k -> Text.toCaseFold k `elem` map Text.toCaseFold ks)

-- | Drop these keys (compared case-insensitively).
ignore :: [Text] -> Env -> Env
ignore ks = filterKeys (\k -> Text.toCaseFold k `notElem` map Text.toCaseFold ks)

-- | Keep only keys satisfying a predicate (applied in order with other key transformations).
filterKeys :: (Text -> Bool) -> Env -> Env
filterKeys f e = e {envChain = envChain e ++ [\k -> if f k then Just k else Nothing]}

-- | Transform keys (applied in order with other key transformations).
mapKeys :: (Text -> Text) -> Env -> Env
mapKeys f e = e {envChain = envChain e ++ [Just . f]}

-- | Put the values in a profile (default 'Default').
inProfile :: Profile -> Env -> Env
inProfile p e = e {envProfile = p}

-- | Put the values in the 'Global' profile, overriding every profile.
global :: Env -> Env
global = inProfile Global

-- | Do not parse values: every value is a string.
unparsed :: Env -> Env
unparsed e = e {envParsed = False}

-- | Read from the given variables instead of the process environment
-- (deterministic tests, or environments assembled by hand).
fromList :: [(Text, Text)] -> Env -> Env
fromList vs e = e {envSnapshot = Just vs}

-- | The variable name that sets a key path, assuming no custom
-- 'mapKeys' (used in error messages and missing-field hints).
envVariableName :: Env -> Path -> Text
envVariableName e p =
  envPrefix e <> Text.intercalate (fromMaybe "." (envSplit e)) [Text.toUpper k | Key k <- segments p]

keyOf :: Env -> Text -> Maybe [Text]
keyOf e name = do
  rest <- stripPrefixCI (envPrefix e) name
  k <- foldl (\acc f -> acc >>= f) (Just rest) (envChain e)
  let k' = if envLower e then Text.toLower k else k
      segs = Text.splitOn "." k'
  if Text.null k' || any Text.null segs then Nothing else Just segs

stripPrefixCI :: Text -> Text -> Maybe Text
stripPrefixCI p t
  | Text.toCaseFold (Text.take (Text.length p) t) == Text.toCaseFold p = Just (Text.drop (Text.length p) t)
  | otherwise = Nothing

instance Provider Env where
  metadata e =
    withInterpolater (\_ p -> envVariableName e p) $
      named (if Text.null (envPrefix e) then "environment variables" else "environment variables (prefix " <> envPrefix e <> ")")
  provide lim e = do
    vars <- case envSnapshot e of
      Just vs -> pure vs
      Nothing -> map (\(k, v) -> (Text.pack k, Text.pack v)) <$> getEnvironment
    let relevant =
          [ (name, ks, val)
          | (name, val) <- sortOn fst vars
          , not (envIgnoreEmpty e && Text.null val)
          , Just ks <- [keyOf e name]
          ]
        parsed = [ (name, ks, ) <$> parse val | (name, ks, val) <- relevant ]
        parse val
          | envParsed e = parseValue lim val
          | otherwise = Right (Value untagged {tagRaw = Just val} (String val))
        failures = [ err | Left err <- parsed ]
    pure $ case fromErrorList failures of
      Just es -> Left es
      Nothing -> Map.singleton (envProfile e) <$> nest RejectDuplicates [x | Right x <- parsed]

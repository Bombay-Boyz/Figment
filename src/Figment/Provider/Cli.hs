-- | Command-line arguments as a provider.
--
-- Rules (ADR 010, revised):
--
-- * @--key=value@ sets @key@; the value is parsed like an environment
--   variable (numbers, booleans, arrays, ...; original text kept).
-- * @--flag@ sets @flag = true@; @--no-flag@ sets @flag = false@.
-- * Dots nest (@--db.port=5432@) and dashes become underscores
--   (@--max-connections@ is @max_connections@); keys are not
--   lower-cased.
-- * @--@ ends option parsing. Anything else (including @-x@ short
--   options) is positional and returned by 'parseArgs'; the provider
--   ignores it.
-- * @--key value@ is /not/ supported: without a schema it is ambiguous
--   with a flag followed by a positional argument. For rich CLIs, parse
--   with optparse-applicative and feed the result in via
--   'Figment.Aeson.defaults'.
-- * Repeating a key: the last occurrence wins. A malformed option
--   (@--=x@, @--a..b=1@) is an error, not silently ignored.
module Figment.Provider.Cli
  ( Cli
  , cli
  , cliFromProcess
  , keyMap
  , unparsedCli
  , cliInProfile
  , parseArgs
  , Arg (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getArgs)

import Figment.Core
import Figment.Error
import Figment.Internal.Nest
import Figment.Internal.ValueParser (parseValue)
import Figment.Limits
import Figment.Metadata
import Figment.Path
import Figment.Profile
import Figment.Value

-- | Command-line arguments as a configuration source (see the module header for the rules).
data Cli = Cli
  { cliArgs :: [Text]
  , cliKeyMap :: Text -> Text
  , cliParsed :: Bool
  , cliProfile :: Profile
  }

-- | A CLI provider over the given arguments.
cli :: [Text] -> Cli
cli args = Cli args (Text.replace "-" "_") True Default

-- | The arguments of the running process.
cliFromProcess :: IO Cli
cliFromProcess = cli . map Text.pack <$> getArgs

-- | How each key segment is mapped (default: dashes to underscores).
keyMap :: (Text -> Text) -> Cli -> Cli
keyMap f c = c {cliKeyMap = f}

-- | Do not parse values: every value is a string.
unparsedCli :: Cli -> Cli
unparsedCli c = c {cliParsed = False}

-- | Put the values in a profile other than 'Default'.
cliInProfile :: Profile -> Cli -> Cli
cliInProfile p c = c {cliProfile = p}

-- | One classified argument.
data Arg
  = Option !Text ![Text] !(Maybe Text)
    -- ^ original argument, key segments, value (@Nothing@: a flag)
  | NegatedFlag !Text ![Text]
  | Positional !Text
  | Malformed !Text
  deriving stock (Eq, Show)

-- | Classify every argument.
parseArgs :: Cli -> [Arg]
parseArgs c = go (cliArgs c)
  where
    go [] = []
    go ("--" : rest) = map Positional rest
    go (a : rest) = classify a : go rest
    classify a = case Text.stripPrefix "--" a of
      Nothing -> Positional a
      Just body ->
        let (k, eqv) = Text.breakOn "=" body
            v = if Text.null eqv then Nothing else Just (Text.drop 1 eqv)
        in case keySegs k of
             Nothing -> Malformed a
             Just segs -> case (v, Text.stripPrefix "no-" k) of
               (Nothing, Just positive) | Just segs' <- keySegs positive -> NegatedFlag a segs'
               _ -> Option a segs v
    keySegs k
      | Text.null k = Nothing
      | otherwise =
          let segs = map (cliKeyMap c) (Text.splitOn "." k)
          in if any Text.null segs then Nothing else Just segs

instance Provider Cli where
  metadata _ =
    withInterpolater (\_ p -> "--" <> Text.intercalate "." [Text.replace "_" "-" k | Key k <- segments p]) $
      named "command-line arguments"
  provide lim c = pure $
    let args = parseArgs c
        malformed = [ atPath root (configError (SyntaxError ("malformed option `" <> a <> "`"))) | Malformed a <- args ]
        entries = concatMap (entry lim c) args
        failures = malformed ++ [e | Left e <- entries]
    in case fromErrorList failures of
         Just es -> Left es
         Nothing -> Map.singleton (cliProfile c) <$> nest LastWins [x | Right x <- entries]

entry :: Limits -> Cli -> Arg -> [Either ConfigError (Text, [Text], Value)]
entry lim c = \case
  Option a segs (Just v)
    | cliParsed c -> [(a, segs, ) <$> parseValue lim v]
    | otherwise -> [Right (a, segs, Value untagged {tagRaw = Just v} (String v))]
  Option a segs Nothing -> [Right (a, segs, flag True)]
  NegatedFlag a segs -> [Right (a, segs, flag False)]
  _ -> []
  where
    flag b = Value untagged {tagRaw = Just (if b then "true" else "false")} (Bool b)

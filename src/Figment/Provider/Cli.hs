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

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getArgs)

import Figment.Core
import Figment.Error
import Figment.Internal.Nest
import Figment.Internal.Text (isUndecodable)
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
  , cliInvalid :: [Int]
    -- ^ positions (from 1) of arguments that are not valid UTF-8
  }

-- | A CLI provider over the given arguments.
cli :: [Text] -> Cli
cli args = Cli args (Text.replace "-" "_") True Default []

-- | The arguments of the running process.
cliFromProcess :: IO Cli
cliFromProcess = do
  raw <- getArgs
  -- bytes that are not valid UTF-8 arrive as U+DC80..U+DCFF, which
  -- 'Text.pack' would silently replace; remember them so they are reported
  let invalid = [i | (i, a) <- zip [1 ..] raw, any isUndecodable a]
  pure (cli (map Text.pack raw)) {cliInvalid = invalid}

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
        -- messages name an option, never echo its value (it may be a secret)
        malformed = [ atPath root (configError (SyntaxError ("malformed option `" <> argName a <> "`"))) | Malformed a <- args ]
        invalid = [ atPath root (configError (SyntaxError ("command-line argument " <> Text.pack (show i) <> " is not valid UTF-8 (or the process locale is not a UTF-8 locale)"))) | i <- cliInvalid c ]
        entries = concatMap (entry lim c) args
        failures = malformed ++ invalid ++ [e | Left e <- entries]
    in case fromErrorList failures of
         Just es -> Left es
         Nothing -> Map.singleton (cliProfile c) <$> nest LastWins [x | Right x <- entries]

-- | The option as the user typed it, without any @=value@: what error
-- messages call the argument.
argName :: Text -> Text
argName = Text.takeWhile (/= '=')

entry :: Limits -> Cli -> Arg -> [Either ConfigError (Text, [Text], Value)]
entry lim c = \case
  Option a segs (Just v)
    | cliParsed c -> [(argName a, segs, ) <$> first (at segs) (parseValue lim v)]
    | otherwise -> [Right (argName a, segs, Value untagged {tagRaw = Just v} (String v))]
  Option a segs Nothing -> [Right (argName a, segs, flag True)]
  NegatedFlag a segs -> [Right (argName a, segs, flag False)]
  _ -> []
  where
    flag b = Value untagged {tagRaw = Just (if b then "true" else "false")} (Bool b)
    -- errors about one option are located at its key
    at segs = atPath (fromSegments (map Key segs))

-- | Command-line arguments.
module Figment.Test.Cli (tests) where

import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import Figment.Provider.Cli (Arg (..), parseArgs)
import qualified Figment.Value as V
import Figment.Value (stripTags)

loadCli :: [Text.Text] -> IO (Either ConfigErrors Loaded)
loadCli = load . from . cli

ok :: [Text.Text] -> IO Loaded
ok args = loadCli args >>= either (assertFailure . Text.unpack . renderErrors) pure

at :: Text.Text -> Loaded -> Maybe Value
at k l = stripTags <$> findValue (keyPath k) l

tests :: TestTree
tests =
  testGroup
    "cli"
    [ testCase "REGRESSION C1: values are kept and typed" $ do
        l <- ok ["--host=x", "--port=80"]
        at "host" l @?= Just (V.string "x")
        at "port" l @?= Just (V.number 80)
    , testCase "flags, negated flags, dots and dashes" $ do
        l <- ok ["--verbose", "--no-color", "--db.max-connections=5"]
        at "verbose" l @?= Just (V.bool True)
        at "color" l @?= Just (V.bool False)
        at "db.max_connections" l @?= Just (V.number 5)
    , testCase "last occurrence wins; -- ends options" $ do
        l <- ok ["--a=1", "--a=2", "--", "--b=3"]
        at "a" l @?= Just (V.number 2)
        at "b" l @?= Nothing
    , testCase "positional arguments are classified, not lost" $
        parseArgs (cli ["run", "-x", "--k=v", "--", "--z"])
          @?= [Positional "run", Positional "-x", Option "--k=v" ["k"] (Just "v"), Positional "--z"]
    , testCase "malformed options are errors" $ do
        r <- loadCli ["--=x", "--a..b=1"]
        case r of
          Left es -> length (errorList es) @?= 2
          Right _ -> assertFailure "expected errors"
    , testCase "structural conflicts are errors" $ do
        r <- loadCli ["--db=x", "--db.host=y"]
        case r of
          -- conflicts name the options, never their values (which may be secrets)
          Left es -> map errorKind (errorList es) @?= [KeyConflict "--db" "--db.host"]
          Right _ -> assertFailure "expected a conflict"
    , testCase "keys are not lower-cased" $ do
        l <- ok ["--Name=x"]
        at "Name" l @?= Just (V.string "x")
    ]

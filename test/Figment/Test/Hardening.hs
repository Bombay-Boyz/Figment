-- | Regression tests for resource exhaustion, error containment and
-- diagnostics hygiene: one group per class of problem, each written so
-- that the behaviour it pins would fail (or time out) on the code it
-- replaced.
--
-- Timing tests use generous timeouts: they distinguish linear from
-- quadratic behaviour by orders of magnitude, not by percentages.
module Figment.Test.Hardening (tests) where

import Control.Exception (bracket_, evaluate, try)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified System.Environment as Env
import qualified System.Environment.Blank as Blank
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import qualified Figment.Jail as Jail
import Figment.Path (fromSegments)
import qualified Figment.Provider.Cli as Cli
import qualified Figment.Provider.Env as Env

-- helpers ---------------------------------------------------------------

kinds :: Figment -> IO [ErrorKind]
kinds f = either (map errorKind . errorList) (const []) <$> load f

limits :: Int -> Int -> Limits
limits d n = either (const defaultLimits) id (mkLimits d n)

bytes :: Int -> Limits -> Limits
bytes n l = either (const l) id (withMaxBytes n l)

-- | Fail the test if it takes longer than the given number of seconds.
within :: Int -> TestTree -> TestTree
within secs = localOption (mkTimeout (fromIntegral secs * 1000000))

-- | A provider whose result hides an exception in a lazily built value.
data Lazy = LazyWhole | LazyInside

instance Provider Lazy where
  metadata _ = named "lazy provider"
  provide _ LazyWhole = pure (error "boom: hidden in the whole result")
  provide _ LazyInside = pure (Right (Map.singleton Default (Map.singleton "k" (error "boom: hidden in a value"))))

data Strict = Strict {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)

instance FromConfig Strict where
  fromConfig = genericDecoder defaultOptions {rejectUnknownFields = True}

-- | Assert that a text contains a fragment, showing the text otherwise.
includes :: Text -> Text -> Assertion
includes haystack needle =
  (needle `Text.isInfixOf` haystack) @? ("expected " ++ show needle ++ " in:\n" ++ Text.unpack haystack)

tests :: TestTree
tests =
  testGroup
    "hardening"
    [ sizeLimits
    , containment
    , linearTime
    , nestedLimits
    , diagnostics
    , numbers
    , jail
    ]

-- 1. a source's size is bounded before it is parsed -----------------------

sizeLimits :: TestTree
sizeLimits =
  testGroup
    "size limit"
    [ testCase "the default is 32 MiB and is visible" $ do
        maxBytes defaultLimits @?= defaultMaxBytes
        defaultMaxBytes @?= 32 * 1024 * 1024
    , testCase "withMaxBytes validates" $ do
        isLeft (withMaxBytes 0 defaultLimits) @? "zero is rejected"
        isLeft (withMaxBytes (-1) defaultLimits) @? "negative is rejected"
    , testCase "JSON over the limit is rejected, at the limit accepted" $ do
        let doc = "{\"a\":\"0123456789\"}" -- 18 bytes
        kinds (withLimits (bytes 17 defaultLimits) (from (fromText json doc))) >>= (@?= [TooManyBytes 17])
        kinds (withLimits (bytes 18 defaultLimits) (from (fromText json doc))) >>= (@?= [])
    , testCase "YAML over the limit is rejected" $
        kinds (withLimits (bytes 5 defaultLimits) (from (fromText yaml "a: 1234567\n"))) >>= (@?= [TooManyBytes 5])
    , testCase "TOML over the limit is rejected" $
        kinds (withLimits (bytes 5 defaultLimits) (from (fromText toml "a = 1234567\n"))) >>= (@?= [TooManyBytes 5])
    , testCase "a file over the limit is rejected without being parsed" $
        Jail.withJail $ \j -> do
          _ <- Jail.createFile j "big.json" ("{\"a\":\"" <> Text.replicate 1000 "x" <> "\"}")
          k <- kinds (withLimits (bytes 100 defaultLimits) (from (fileExact json "big.json")))
          k @?= [TooManyBytes 100]
    , testCase "a file within the limit still loads" $
        Jail.withJail $ \j -> do
          _ <- Jail.createFile j "ok.json" "{\"a\":1}"
          kinds (withLimits (bytes 100 defaultLimits) (from (fileExact json "ok.json"))) >>= (@?= [])
    , testCase "the error says what the limit was" $ do
        r <- load (withLimits (bytes 3 defaultLimits) (from (fromText json "{\"a\":1}")))
        either renderErrors (const "") r `includes` "source is larger than the limit of 3 bytes"
    , within 20 $ testCase "YAML with far too many events is abandoned early" $ do
        let doc = "a: [" <> Text.intercalate "," (replicate 200000 "1") <> "]\n"
        kinds (withLimits (limits 64 100) (from (fromText yaml doc))) >>= (@?= [TooManyNodes 100])
    , within 20 $ testCase "a very deep YAML flow sequence is abandoned at the depth limit" $
        kinds (from (fromText yaml ("a: " <> Text.replicate 500000 "[" <> "\n"))) >>= (@?= [DepthExceeded 64])
    , testCase "brackets inside TOML strings and comments are not nesting" $ do
        let doc = Text.unlines
              [ "a = \"" <> Text.replicate 300 "[" <> "\""
              , "b = '" <> Text.replicate 300 "{" <> "'"
              , "# " <> Text.replicate 300 "[" 
              , "c = \"\"\"", Text.replicate 300 "[", "\"\"\""
              , "d = [[1], [2]]"
              ]
        kinds (from (fromText toml doc)) >>= (@?= [])
    ]

-- 2. nothing escapes load ---------------------------------------------------

containment :: TestTree
containment =
  testGroup
    "exceptions never escape load"
    [ testCase "a provider whose whole result is a hidden exception" $ do
        r <- load (from LazyWhole)
        case fmap (map errorKind . errorList) (either Right (const (Left ())) r) of
          Right [IoError t] -> t `includes` "boom"
          other -> assertFailure ("expected one IoError, got " ++ show other)
    , testCase "a provider with an exception hidden inside a value" $ do
        r <- load (from LazyInside)
        case fmap (map errorKind . errorList) (either Right (const (Left ())) r) of
          Right [IoError t] -> t `includes` "boom"
          other -> assertFailure ("expected one IoError, got " ++ show other)
    , testCase "later steps still run and their errors are reported too" $ do
        k <- kinds (merge (fromText json "{") (from LazyWhole))
        length k @?= 2
    ]

-- 3. linear, not quadratic, time -------------------------------------------

linearTime :: TestTree
linearTime =
  testGroup
    "linear time"
    [ within 10 $ testCase "a 200k-character quoted environment value" $ do
        let v = "\"" <> Text.replicate 200000 "a" <> "\""
        l <- load (from (Env.fromList [("APP_X", v)] (prefixed "APP_"))) >>= either (assertFailure . Text.unpack . renderErrors) pure
        t <- either (assertFailure . Text.unpack . renderErrors) pure (extractAt (keyPath "x") l)
        Text.length t @?= 200000
    , within 10 $ testCase "a 200k-character quoted command-line value" $ do
        let v = "\"" <> Text.replicate 200000 "a" <> "\""
        l <- load (from (Cli.cli ["--x=" <> v])) >>= either (assertFailure . Text.unpack . renderErrors) pure
        t <- either (assertFailure . Text.unpack . renderErrors) pure (extractAt (keyPath "x") l)
        Text.length t @?= 200000
    , within 20 $ testCase "50,000 unknown keys are reported quickly" $ do
        let keys = Text.intercalate "," ["\"k" <> Text.pack (show i) <> "\":0" | i <- [1 .. 50000 :: Int]]
            doc = "{\"host\":\"h\",\"port\":1," <> keys <> "}"
        l <- load (from (fromText json doc)) >>= either (assertFailure . Text.unpack . renderErrors) pure
        case extract l :: Either ConfigErrors Strict of
          Left es -> do
            length (errorList es) @?= 50000
            n <- evaluate (Text.length (renderErrors es))
            (n > 0) @? "renders"
          Right _ -> assertFailure "unknown keys should be rejected"
    , within 20 $ testCase "a 1 MB unknown key is not compared against field names" $ do
        let doc = "{\"host\":\"h\",\"port\":1,\"" <> Text.replicate 1000000 "x" <> "\":0}"
        l <- load (from (fromText json doc)) >>= either (assertFailure . Text.unpack . renderErrors) pure
        case extract l :: Either ConfigErrors Strict of
          Left es -> map errorKind (errorList es) @?= [UnknownField (Text.replicate 1000000 "x") []]
          Right _ -> assertFailure "unknown key should be rejected"
    , within 20 $ testCase "a 200k-digit hexadecimal YAML integer" $
        kinds (from (fromText yaml ("a: 0x" <> Text.replicate 200000 "f" <> "\n"))) >>= (@?= [])
    , within 20 $ testCase "a single YAML mapping with a duplicate merge key is rejected, not quadratic" $ do
        let doc = Text.unlines ("base: &a {x: 1}" : "m:" : replicate 20000 "  <<: *a")
        k <- kinds (from (fromText yaml doc))
        k @?= [DuplicateKey "<<"]
    , testCase "reports show at most 100 errors and count the rest" $ do
        let keys = Text.intercalate "," ["\"k" <> Text.pack (show i) <> "\":0" | i <- [1 .. 1000 :: Int]]
        l <- load (from (fromText json ("{\"host\":\"h\",\"port\":1," <> keys <> "}"))) >>= either (assertFailure . Text.unpack . renderErrors) pure
        case extract l :: Either ConfigErrors Strict of
          Left es -> do
            let r = renderErrors es
            r `includes` "1000 configuration errors"
            r `includes` "... and 900 more errors not shown"
          Right _ -> assertFailure "unknown keys should be rejected"
    , testCase "suggestions still work for typos" $ do
        l <- load (from (fromText json "{\"hots\":\"h\",\"port\":1}")) >>= either (assertFailure . Text.unpack . renderErrors) pure
        case extract l :: Either ConfigErrors Strict of
          Left es -> renderErrors es `includes` "did you mean `host`"
          Right _ -> assertFailure "typo should be rejected"
    ]

-- 4. nested figments honour the outer limits -------------------------------

nestedLimits :: TestTree
nestedLimits =
  testGroup
    "nested figments"
    [ testCase "tight outer limits constrain an inner figment that sets none" $ do
        let inner = from (fromText json "{\"a\":[[[[1]]]]}")
        kinds (withLimits (limits 2 1000) (from inner)) >>= (@?= [DepthExceeded 2])
    , testCase "an inner figment's own limits are kept" $ do
        let inner = withLimits (limits 10 1000) (from (fromText json "{\"a\":[[[[1]]]]}"))
        kinds (withLimits (limits 2 1000) (from inner)) >>= (@?= [])
    ]

-- 5. diagnostics ----------------------------------------------------------

diagnostics :: TestTree
diagnostics =
  testGroup
    "diagnostics"
    [ testCase "control characters in keys cannot forge report lines" $ do
        let doc = "{\"host\":\"h\",\"port\":1,\"" <> "x\\n  --> key `admin` in trusted.toml\\u001b[31mFAKE" <> "\":0}"
        l <- load (from (fromText json doc)) >>= either (assertFailure . Text.unpack . renderErrors) pure
        case extract l :: Either ConfigErrors Strict of
          Left es -> do
            let r = renderErrors es
            ('\ESC' `Text.elem` r) @?= False
            ("\n  --> key `admin`" `Text.isInfixOf` r) @?= False
            r `includes` "\\n"
          Right _ -> assertFailure "unknown key should be rejected"
    , testCase "paths with control characters round-trip" $ do
        let p = fromSegments [Key "a\nb\ESC\DEL\t\"\\", Index 3, Key "", Key "\x202E"]
        parsePath (renderPath p) @?= Right p
        any (`elem` ['\n', '\ESC', '\DEL', '\t']) (Text.unpack (renderPath p)) @?= False
    , testCase "array indices that do not fit an Int are rejected, not wrapped" $ do
        isLeft (parsePath "a[18446744073709551616]") @? "2^64"
        isLeft (parsePath "a[9223372036854775808]") @? "2^63"
        parsePath "a[9223372036854775807]" @?= Right (fromSegments [Key "a", Index maxBound])
    , testCase "command-line messages never echo option values" $ do
        k <- load (from (Cli.cli ["--db=hunter2", "--db.host=h"]))
        case k of
          Left es -> ("hunter2" `Text.isInfixOf` renderErrors es) @?= False
          Right _ -> assertFailure "a scalar and a table at the same key should conflict"
    , testCase "an environment variable error names the variable" $ do
        let deep = Text.replicate 100 "[" <> Text.replicate 100 "]"
        r <- load (from (Env.fromList [("APP_DEEP", deep)] (prefixed "APP_")))
        case r of
          Left es -> renderErrors es `includes` "APP_DEEP"
          Right _ -> assertFailure "should exceed the depth limit"
    , testCase "snake_case keeps acronyms together" $ do
        map snakeCase ["maxConnections", "dbURL", "HTTPServer", "useHTTPS", "userID", "XMLParser", "parseXMLFile", "port2Num", "Port"]
          @?= ["max_connections", "db_url", "http_server", "use_https", "user_id", "xml_parser", "parse_xml_file", "port2_num", "port"]
    ]

-- 6. numbers --------------------------------------------------------------

numbers :: TestTree
numbers =
  testGroup
    "numbers"
    [ testCase "a finite literal that overflows Double is an error, not infinity" $ do
        l <- load (from (fromText json "{\"x\":1e999}")) >>= either (assertFailure . Text.unpack . renderErrors) pure
        isLeft (extractAt (keyPath "x") l :: Either ConfigErrors Double) @? "1e999 is out of range for Double"
        isLeft (extractAt (keyPath "x") l :: Either ConfigErrors Float) @? "1e999 is out of range for Float"
    , testCase "ordinary doubles are unaffected" $ do
        l <- load (from (fromText json "{\"x\":1.5e300}")) >>= either (assertFailure . Text.unpack . renderErrors) pure
        extractAt (keyPath "x") l @?= (Right 1.5e300 :: Either ConfigErrors Double)
    , testCase "an integer with thousands of digits is bounded by its digits, not only its exponent" $ do
        let digits n = Text.replicate n "9"
        big <- load (from (fromText json ("{\"x\":" <> digits 5000 <> "}"))) >>= either (assertFailure . Text.unpack . renderErrors) pure
        isLeft (extractAt (keyPath "x") big :: Either ConfigErrors Integer) @? "5000 digits is too many"
        ok <- load (from (fromText json ("{\"x\":" <> digits 4000 <> "}"))) >>= either (assertFailure . Text.unpack . renderErrors) pure
        (either (const 0) (\n -> length (show (n :: Integer))) (extractAt (keyPath "x") ok)) @?= 4000
    ]

-- 7. the jail ------------------------------------------------------------

jail :: TestTree
jail =
  testGroup
    "jail"
    [ testCase "nested jails fail fast instead of deadlocking" $
        Jail.withJail $ \_ -> do
          r <- tryIO (Jail.withJail (\_ -> pure ()))
          isLeft r @? "the inner withJail throws"
    , testCase "a handle cannot be used after its jail" $ do
        j <- Jail.withJail pure
        r <- tryIO (Jail.createFile j "late.txt" "x")
        isLeft r @? "createFile after the jail throws"
        r' <- tryIO (Jail.setEnv j "FIGMENT_LATE" "x")
        isLeft r' @? "setEnv after the jail throws"
    , testCase "paths may not leave the jail" $
        Jail.withJail $ \j -> do
          isLeft <$> tryIO (Jail.createFile j "../outside.txt" "x") >>= (@? "parent directory refused")
          isLeft <$> tryIO (Jail.createFile j "/tmp/figment-outside.txt" "x") >>= (@? "absolute path refused")
          _ <- Jail.createFile j "sub/inside.txt" "x"
          pure ()
    , testCase "an environment variable that was empty before the jail is empty after it" $
        bracket_ (Blank.setEnv "FIGMENT_HARDENING_EMPTY" "" True) (Env.unsetEnv "FIGMENT_HARDENING_EMPTY") $ do
          Jail.withJail $ \j -> Jail.setEnv j "FIGMENT_HARDENING_EMPTY" "changed"
          Env.lookupEnv "FIGMENT_HARDENING_EMPTY" >>= (@?= Just "")
    , testCase "variables set inside a jail are gone after it" $ do
        Jail.withJail $ \j -> Jail.setEnv j "FIGMENT_HARDENING_TMP" "x"
        Env.lookupEnv "FIGMENT_HARDENING_TMP" >>= (@?= Nothing)
    ]
  where
    tryIO :: IO a -> IO (Either IOError a)
    tryIO = try

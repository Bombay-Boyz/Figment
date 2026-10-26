-- | File formats end to end.
module Figment.Test.Providers (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime, ZonedTime, zonedTimeToUTC, UTCTime)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import qualified Figment.Jail as Jail

data Cfg = Cfg {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

expectOk :: Figment -> IO Loaded
expectOk f = load f >>= either (assertFailure . Text.unpack . renderErrors) pure

expectErr :: Figment -> IO [ConfigError]
expectErr f = load f >>= either (pure . errorList) (const (assertFailure "expected a load error"))

kinds :: [ConfigError] -> [ErrorKind]
kinds = map errorKind

tests :: TestTree
tests =
  testGroup
    "providers"
    [ testGroup "files" fileTests
    , testGroup "json" jsonTests
    , testGroup "toml" tomlTests
    , testGroup "yaml" yamlTests
    , testGroup "serialized" serializedTests
    ]

fileTests :: [TestTree]
fileTests =
  [ testCase "round trip in every format" $ Jail.withJail $ \j -> do
      _ <- Jail.createFile j "a.json" "{\"host\":\"j\",\"port\":1}"
      _ <- Jail.createFile j "a.toml" "host = \"t\"\nport = 2\n"
      _ <- Jail.createFile j "a.yaml" "host: y\nport: 3\n"
      mapM_
        (\(f, expected) -> expectOk (from f) >>= \l -> extract l @?= Right expected)
        [ (file json "a.json", Cfg "j" 1)
        , (file toml "a.toml", Cfg "t" 2)
        , (file yaml "a.yaml", Cfg "y" 3)
        ]
  , testCase "a missing optional file contributes nothing" $ Jail.withJail $ \_ -> do
      l <- expectOk (from (file toml "absent.toml"))
      extract l @?= (Right Map.empty :: Either ConfigErrors (Map.Map Text Int))
  , testCase "a missing required file is an IoError naming the file" $ Jail.withJail $ \_ -> do
      es <- expectErr (from (required (file toml "absent.toml")))
      case kinds es of
        [IoError msg] -> assertBool (Text.unpack msg) ("absent.toml" `Text.isInfixOf` msg)
        other -> assertFailure (show other)
  , testCase "relative files are searched for in parent directories" $ Jail.withJail $ \j -> do
      _ <- Jail.createFile j "App.toml" "host = \"up\"\nport = 1\n"
      sub <- Jail.createDirectory j "a/b/c"
      Jail.inDirectory j sub $ do
        l <- expectOk (from (file toml "App.toml"))
        extract l @?= Right (Cfg "up" 1)
        fmap metaSource (findMetadata (keyPath "host") l) @?= Just (Just (FileSource (Jail.jailDirectory j </> "App.toml")))
        l2 <- expectOk (from (fileExact toml "App.toml"))
        (extract l2 :: Either ConfigErrors (Map.Map Text Text)) @?= Right Map.empty
  , testCase "top level must be a dictionary" $ do
      es <- expectErr (from (fromText json "[1,2]"))
      kinds es @?= [InvalidType "an array of length 2" "a dictionary at the top level"]
  , testCase "nested files: top-level keys are profiles" $ do
      l <- expectOk (from (nested (fromText yaml "default:\n  host: d\n  port: 1\nprod:\n  host: p\n")))
      extract (reselect "prod" l) @?= Right (Cfg "p" 1)
      es <- expectErr (from (nested (fromText yaml "default: 5\n")))
      kinds es @?= [InvalidType "integer 5" "a dictionary (nested files map profile names to dictionaries)"]
  ]

jsonTests :: [TestTree]
jsonTests =
  [ testCase "duplicate keys are errors" $ do
      es <- expectErr (from (fromText json "{\"a\":1,\"a\":2}"))
      kinds es @?= [DuplicateKey "a"]
      map (renderPath . errorPath) es @?= ["a"]
  , testCase "syntax errors and trailing garbage" $ do
      es <- expectErr (from (fromText json "{\"a\":1} x"))
      case kinds es of
        [SyntaxError _] -> pure ()
        other -> assertFailure (show other)
  , testCase "big and precise numbers survive exactly" $ do
      l <- expectOk (from (fromText json "{\"n\":123456789012345678901234567890.123456789}"))
      fmap (valueNode) (findValue (keyPath "n") l) @?= Just (Num (Finite 123456789012345678901234567890.123456789))
  ]

tomlTests :: [TestTree]
tomlTests =
  [ testCase "dates and times decode into Data.Time types" $ do
      l <- expectOk (from (fromText toml "d = 2024-01-02\nlt = 2024-01-02T03:04:05\nzt = 2024-01-02T03:04:05+05:30\n"))
      extractAt (keyPath "d") l @?= Right (read "2024-01-02" :: Day)
      extractAt (keyPath "lt") l @?= Right (read "2024-01-02 03:04:05" :: LocalTime)
      fmap zonedTimeToUTC (extractAt (keyPath "zt") l :: Either ConfigErrors ZonedTime)
        @?= Right (read "2024-01-01 21:34:05 UTC" :: UTCTime)
      extractAt (keyPath "zt") l @?= Right (read "2024-01-01 21:34:05 UTC" :: UTCTime)
  , testCase "inf and nan are kept, not corrupted" $ do
      l <- expectOk (from (fromText toml "a = inf\nb = -inf\nc = nan\n"))
      fmap valueNode (findValue (keyPath "a") l) @?= Just (Num PosInfinity)
      fmap valueNode (findValue (keyPath "b") l) @?= Just (Num NegInfinity)
      fmap valueNode (findValue (keyPath "c") l) @?= Just (Num NaN)
      (extractAt (keyPath "a") l :: Either ConfigErrors Double) @?= Right (1 / 0)
      case extractAt (keyPath "a") l :: Either ConfigErrors Int of
        Left _ -> pure ()
        Right v -> assertFailure (show v)
  , testCase "invalid UTF-8 is rejected (not replaced)" $ Jail.withJail $ \j -> do
      p <- Jail.createBinaryFile j "bad.toml" "name = \"\xff\xfe\"\n"
      es <- expectErr (from (fileExact toml p))
      case kinds es of
        [SyntaxError msg] -> assertBool (Text.unpack msg) ("UTF-8" `Text.isInfixOf` msg)
        other -> assertFailure (show other)
  , testCase "syntax errors carry a position" $ do
      es <- expectErr (from (fromText toml "a = 1\nb = = 2\n"))
      map errorPos es @?= [Just (Pos 2 5)]
  , testCase "redefinition is a duplicate key" $ do
      es <- expectErr (from (fromText toml "a = 1\na = 2\n"))
      kinds es @?= [DuplicateKey "a"]
  ]

yamlTests :: [TestTree]
yamlTests =
  [ testCase "YAML 1.2 core schema: no Norway problem" $ do
      l <- expectOk (from (fromText yaml "country: NO\nflag: on\nyes_: yes\nt: true\nn: ~\ni: 0x1F\nf: 1.5e3\nq: '42'\n"))
      let node k = fmap valueNode (findValue (keyPath k) l)
      node "country" @?= Just (String "NO")
      node "flag" @?= Just (String "on")
      node "yes_" @?= Just (String "yes")
      node "t" @?= Just (Bool True)
      node "n" @?= Just Null
      node "i" @?= Just (Num (Finite 31))
      node "f" @?= Just (Num (Finite 1500))
      node "q" @?= Just (String "42")
  , testCase "anchors, aliases and merge keys" $ do
      l <- expectOk (from (fromText yaml "base: &b\n  host: h\n  port: 1\nsite:\n  <<: *b\n  port: 2\ncopy: *b\n"))
      extractAt (keyPath "site") l @?= Right (Cfg "h" 2)
      extractAt (keyPath "copy") l @?= Right (Cfg "h" 1)
  , testCase "positions are one-based line and column" $ do
      l <- expectOk (from (fromText yaml "a: 1\nb:\n  c: x\n"))
      fmap (tagPos . valueTag) (findValue (keyPath "b.c") l) @?= Just (Just (Pos 3 6))
  , testCase "duplicate keys and multiple documents are errors" $ do
      es <- expectErr (from (fromText yaml "a: 1\na: 2\n"))
      kinds es @?= [DuplicateKey "a"]
      es2 <- expectErr (from (fromText yaml "a: 1\n---\nb: 2\n"))
      case kinds es2 of
        [SyntaxError m] -> assertBool (Text.unpack m) ("multiple" `Text.isInfixOf` m)
        other -> assertFailure (show other)
  , testCase "an empty file is an empty configuration" $ do
      l <- expectOk (from (fromText yaml ""))
      (extract l :: Either ConfigErrors (Map.Map Text Int)) @?= Right Map.empty
  ]

serializedTests :: [TestTree]
serializedTests =
  [ testCase "defaults, keyed values and globals" $ do
      l <- expectOk $
        new
          `andThenMerge` defaults (Map.fromList [("host" :: Text, "d" :: Text)])
          `andThenMerge` keyed "port" (5 :: Int)
      extract l @?= Right (Cfg "d" 5)
      l2 <- expectOk (from (defaults (Map.fromList [("port" :: Text, 1 :: Int)])) <> from (globals (Map.fromList [("port" :: Text, 2 :: Int)])))
      extractAt (keyPath "port") (reselect "any" l2) @?= Right (2 :: Int)
  , testCase "serialized values point at the Haskell source line" $ do
      l <- expectOk (from (defaults (Map.fromList [("host" :: Text, "d" :: Text)])))
      case findMetadata (keyPath "host") l >>= metaSource of
        Just (CodeSource _) -> pure ()
        other -> assertFailure (show other)
  , testCase "a non-dictionary needs keyed" $ do
      es <- expectErr (from (defaults (5 :: Int)))
      case kinds es of
        [InvalidType _ _] -> pure ()
        other -> assertFailure (show other)
  ]
  where
    andThenMerge f p = merge p f

{-# LANGUAGE DerivingVia #-}
-- Sum types with record payloads are under test here, deliberately.
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | The native decoder.
module Figment.Test.Decode (tests) where

import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.Int (Int8)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, fromGregorian)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import qualified Figment.Jail as Jail
import qualified Figment.Provider.Env as Env

data Server = Server {host :: Text, port :: Word16, tags :: [Text]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

data Level = Debug | Info | Warn
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

data Shape = Circle {radius :: Double} | Square {side :: Double} | Dot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

newtype Port = Port Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

data Pair = Pair Int Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

data Strict = Strict {maxConnections :: Int, timeoutSecs :: Int}
  deriving stock (Eq, Show, Generic)

instance FromConfig Strict where
  fromConfig = genericDecoder defaultOptions {fieldLabelModifier = snakeCase, rejectUnknownFields = True}

data Opt = Opt {required' :: Int, optional' :: Maybe Int, withDefault :: Int}
  deriving stock (Eq, Show, Generic)

instance FromConfig Opt where
  fromConfig = Opt <$> field "required" <*> optionalField "optional" <*> fieldOr "with_default" 42

newtype Legacy = Legacy (Map.Map Text Int)
  deriving stock (Eq, Show)
  deriving newtype (Aeson.FromJSON)
  deriving (FromConfig) via (ViaAeson Legacy)

data WithTagged = WithTagged {secret :: Tagged Text, dir :: RelativePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

loadText :: Data -> IO Loaded
loadText d = load (from d) >>= either (assertFailure . Text.unpack . renderErrors) pure

decodeJ :: FromConfig a => Text -> IO (Either ConfigErrors a)
decodeJ t = extract <$> loadText (fromText json t)

paths :: ConfigErrors -> [Text]
paths = map (renderPath . errorPath) . errorList

tests :: TestTree
tests =
  testGroup
    "decode"
    [ testCase "records, lists, bounded ints" $ do
        r <- decodeJ "{\"host\":\"h\",\"port\":8080,\"tags\":[\"a\",\"b\"]}"
        r @?= Right (Server "h" 8080 ["a", "b"])
    , testCase "REGRESSION C3: the error points at the failing field, not the first key" $ do
        r <- decodeJ "{\"host\":\"h\",\"port\":\"bad\",\"tags\":[]}" :: IO (Either ConfigErrors Server)
        either paths (const []) r @?= ["port"]
    , testCase "all errors are reported at once, each with its path" $ do
        r <- decodeJ "{\"port\":70000,\"tags\":[\"ok\", 5]}" :: IO (Either ConfigErrors Server)
        case r of
          Left es -> do
            paths es @?= ["host", "port", "tags[1]"]
            map errorKind (errorList es)
              @?= [ MissingField "host"
                  , InvalidValue "integer 70000" "an integer in the range 0..65535"
                  , InvalidType "integer 5" "a string"
                  ]
          Right v -> assertFailure (show v)
    , testCase "errors carry the source, line and column" $ do
        l <- loadText (fromText toml "host = \"h\"\nport = \"x\"\ntags = []\n")
        case extract l :: Either ConfigErrors Server of
          Left es -> case errorList es of
            [e] -> do
              errorPos e @?= Just (Pos 2 8)
              fmap metaName (errorMetadata e) @?= Just "TOML source string"
            other -> assertFailure (show other)
          Right v -> assertFailure (show v)
    , testCase "out of range never wraps" $ do
        r <- decodeJ "{\"n\":300}" :: IO (Either ConfigErrors (Map.Map Text Int8))
        either (map errorKind . errorList) (const []) r @?= [InvalidValue "integer 300" "an integer in the range -128..127"]
    , testCase "non-integers are not truncated" $ do
        r <- decodeJ "{\"n\":1.5}" :: IO (Either ConfigErrors (Map.Map Text Int))
        either (map errorKind . errorList) (const []) r @?= [InvalidValue "number 1.5" "an integer"]
    , testCase "enums, case-insensitively; unknown variants list the options" $ do
        r <- decodeJ "{\"a\":\"Debug\",\"b\":\"warn\"}"
        r @?= Right (Map.fromList [("a" :: Text, Debug), ("b", Warn)])
        bad <- decodeJ "{\"a\":\"verbose\"}" :: IO (Either ConfigErrors (Map.Map Text Level))
        either (map errorKind . errorList) (const []) bad @?= [UnknownVariant "verbose" ["Debug", "Info", "Warn"]]
    , testCase "sum types are externally tagged" $ do
        r <- decodeJ "{\"xs\":[{\"Circle\":{\"radius\":1}}, {\"Square\":{\"side\":2}}, \"Dot\"]}"
        r @?= Right (Map.fromList [("xs" :: Text, [Circle 1, Square 2, Dot])])
    , testCase "newtypes unwrap, positional constructors are arrays" $ do
        p <- decodeJ "{\"p\":80}"
        p @?= Right (Map.fromList [("p" :: Text, Port 80)])
        q <- decodeJ "{\"q\":[1,\"x\"]}"
        q @?= Right (Map.fromList [("q" :: Text, Pair 1 "x")])
    , testCase "snake_case options and unknown-field suggestions" $ do
        ok <- decodeJ "{\"max_connections\":1,\"timeout_secs\":2}"
        ok @?= Right (Strict 1 2)
        bad <- decodeJ "{\"max_conections\":1,\"timeout_secs\":2}" :: IO (Either ConfigErrors Strict)
        case bad of
          Left es -> map errorKind (errorList es) @?= [UnknownField "max_conections" ["max_connections"], MissingField "max_connections"]
          Right v -> assertFailure (show v)
    , testCase "hand-written decoders: optional and defaulted fields" $ do
        r <- decodeJ "{\"required\":1}"
        r @?= Right (Opt 1 Nothing 42)
    , testCase "missing-field hints name every way to set it" $ do
        l <- load (new & merge (fromText json "{}") & merge (Env.fromList [] (prefixed "APP_") & Env.split "__") & merge (cli []))
               >>= either (assertFailure . show) pure
        case extract l :: Either ConfigErrors Server of
          Left es -> case filter ((== MissingField "port") . errorKind) (errorList es) of
            [e] -> do
              let h = Text.unlines (errorHints e)
              assertBool (Text.unpack h) ("`APP_PORT`" `Text.isInfixOf` h && "`--port`" `Text.isInfixOf` h)
            other -> assertFailure (show other)
          Right v -> assertFailure (show v)
    , testCase "lossy extraction" $ do
        l <- loadText (fromText json "{\"host\":5,\"port\":\"8080\",\"tags\":[]}")
        case extract l :: Either ConfigErrors Server of
          Left es -> length (errorList es) @?= 2
          Right v -> assertFailure (show v)
        extractLossy l @?= Right (Server "5" 8080 [])
    , testCase "Tagged and RelativePath know their source" $ withTmpToml $ \path -> do
        l <- load (from (fileExact toml path)) >>= either (assertFailure . show) pure
        case extract l of
          Right (WithTagged s d) -> do
            taggedValue s @?= "x"
            taggedPath s @?= keyPath "secret"
            fmap metaSource (taggedMetadata s) @?= Just (Just (FileSource path))
            relative d @?= dirOf path <> "/data"
          Left e -> assertFailure (Text.unpack (renderErrors e))
    , testCase "ISO dates, NonEmpty, tuples" $ do
        r <- decodeJ "{\"d\":\"2024-02-29\"}"
        r @?= Right (Map.fromList [("d" :: Text, fromGregorian 2024 2 29 :: Day)])
        ne <- decodeJ "{\"xs\":[]}" :: IO (Either ConfigErrors (Map.Map Text (NonEmpty Int)))
        either (map errorKind . errorList) (const []) ne @?= [InvalidLength 0 "a non-empty array"]
        t <- decodeJ "{\"t\":[1,\"a\"]}"
        t @?= Right (Map.fromList [("t" :: Text, (1 :: Int, "a" :: Text))])
    , testCase "ViaAeson bridge keeps the path of aeson's error" $ do
        ok <- decodeJ "{\"legacy\":{\"a\":1}}"
        ok @?= Right (Map.fromList [("legacy" :: Text, Legacy (Map.fromList [("a", 1)]))])
        bad <- decodeJ "{\"legacy\":{\"a\":\"x\"}}" :: IO (Either ConfigErrors (Map.Map Text Legacy))
        either paths (const []) bad @?= ["legacy.a"]
    , testCase "validate attaches the location of the value" $ do
        l <- loadText (fromText toml "port = 0\n")
        let positive p = if (p :: Int) > 0 then Right p else Left "port must be positive"
            d = fieldWith "port" (validate positive fromConfig) Nothing
        case extractWith d l of
          Left es -> map (\e -> (errorKind e, errorPos e)) (errorList es) @?= [(Message "port must be positive", Just (Pos 1 8))]
          Right v -> assertFailure (show v)
    ]
  where
    dirOf = reverse . drop 1 . dropWhile (/= '/') . reverse

withTmpToml :: (FilePath -> IO a) -> IO a
withTmpToml k = Jail.withJail $ \j ->
  Jail.createFile j "conf/app.toml" "secret = \"x\"\ndir = \"data\"\n" >>= k

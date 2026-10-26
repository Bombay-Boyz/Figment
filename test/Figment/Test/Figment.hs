-- | Loading, profiles, queries, composition.
module Figment.Test.Figment (tests) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Stack (SrcLoc (..))
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import qualified Figment.Provider.Env as Env

data Cfg = Cfg {name :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

tomlProfiles :: Text
tomlProfiles =
  "[default]\nname = \"base\"\nport = 1\n\n[debug]\nname = \"dbg\"\n\n[global]\nport = 9\n"

loadOk :: Figment -> IO Loaded
loadOk f = load f >>= either (assertFailure . Text.unpack . renderErrors) pure

tests :: TestTree
tests =
  testGroup
    "figment"
    [ testCase "default profile, then selected, then global" $ do
        let base = new & merge (nested (fromText toml tomlProfiles))
        l <- loadOk base
        extract l @?= Right (Cfg "base" 9)
        l' <- loadOk (base & select "debug")
        extract l' @?= Right (Cfg "dbg" 9)
        extract (reselect "DEBUG" l) @?= Right (Cfg "dbg" 9)
        extract (reselect "missing" l) @?= Right (Cfg "base" 9)
    , testCase "later providers win, even over globals of earlier ones" $ do
        l <- loadOk $
          new
            & merge (nested (fromText toml tomlProfiles))
            & merge (fromText toml "port = 7")
        -- the second source writes to Default; Global of the first still wins
        extract l @?= Right (Cfg "base" 9)
        l2 <- loadOk $
          new
            & merge (nested (fromText toml tomlProfiles))
            & merge (Env.global (Env.fromList [("APP_PORT", "7")] (prefixed "APP_")))
        extract l2 @?= Right (Cfg "base" 7)
    , testCase "join only fills gaps" $ do
        l <- loadOk $
          new
            & merge (fromText json "{\"name\":\"a\"}")
            & join (fromText json "{\"name\":\"b\",\"port\":2}")
        extract l @?= Right (Cfg "a" 2)
    , testCase "every failing provider is reported, not just the first" $ do
        r <- load $
          new
            & merge (required (fileExact toml "/nonexistent/a.toml"))
            & merge (fromText json "{ not json")
            & merge (fromText yaml "a: [1, 2")
        case r of
          Left es -> length (errorList es) @?= 3
          Right _ -> assertFailure "expected errors"
    , testCase "an exception escaping a provider becomes an error" $ do
        r <- load (from Boom)
        case r of
          Left es -> case errorList es of
            [e] -> do
              errorKind e @?= IoError "boom"
              fmap metaName (errorMetadata e) @?= Just "boom provider"
            other -> assertFailure (show other)
          Right _ -> assertFailure "expected an error"
    , testCase "metadata records where the provider was added" $ do
        l <- loadOk (from (fromText json "{\"name\":\"x\",\"port\":1}"))
        case loadedSources l of
          [m] -> fmap srcLocFile (metaAddedAt m) @?= Just "test/Figment/Test/Figment.hs"
          other -> assertFailure (show other)
    , testCase "findValue / contains / findMetadata / extractAt / focus" $ do
        l <- loadOk (from (fromText json "{\"db\":{\"host\":\"h\",\"port\":5}}"))
        contains (keyPath "db.host") l @?= True
        contains (keyPath "db.nope") l @?= False
        fmap metaName (findMetadata (keyPath "db.port") l) @?= Just "JSON source string"
        extractAt (keyPath "db.port") l @?= Right (5 :: Int)
        extractLossy (focus (keyPath "db") l) @?= Right (Map.fromList [("host" :: Text, "h" :: Text), ("port", "5")])
    , testCase "a Figment is itself a provider, and keeps inner provenance" $ do
        let inner = from (fromText yaml "name: inner\nport: 3\n")
        l <- loadOk (new & merge inner & merge (fromText json "{\"port\":4}"))
        extract l @?= Right (Cfg "inner" 4)
        fmap metaName (findMetadata (keyPath "name") l) @?= Just "YAML source string"
    , testCase "a provider's profile hint is honoured, explicit select wins" $ do
        let hinted = new & merge (Hint "debug") & merge (nested (fromText toml tomlProfiles))
        l <- loadOk hinted
        loadedProfile l @?= "debug"
        l' <- loadOk (hinted & select Default)
        loadedProfile l' @?= Default
    ]

data Boom = Boom

instance Provider Boom where
  metadata _ = named "boom provider"
  provide _ _ = throwIO (ErrorCall "boom")

newtype Hint = Hint Profile

instance Provider Hint where
  metadata _ = named "hint"
  provide _ _ = pure (Right Map.empty)
  providerProfile (Hint p) = Just p

{-# LANGUAGE DuplicateRecordFields #-}

-- | Environment variables.
module Figment.Test.Env (tests) where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import qualified Figment.Jail as Jail
import qualified Figment.Provider.Env as Env
import Figment.Value (stripTags)
import qualified Figment.Value as V

data Cfg = Cfg {host :: Text, port :: Int, max_connections :: Int, zip_code :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

data Db = Db {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

newtype Outer = Outer {db :: Db}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromConfig)

env :: [(Text, Text)] -> Env -> IO (Either ConfigErrors Loaded)
env vars e = load (from (Env.fromList vars e))

ok :: [(Text, Text)] -> Env -> IO Loaded
ok vars e = env vars e >>= either (assertFailure . Text.unpack . renderErrors) pure

node :: Text -> Loaded -> Maybe Node
node k l = valueNode <$> findValue (keyPath k) l

tests :: TestTree
tests =
  testGroup
    "env"
    [ testCase "REGRESSION C1/H1: values are kept and typed" $ do
        l <- ok [("APP_HOST", "x"), ("APP_PORT", "9000"), ("APP_MAX_CONNECTIONS", "5"), ("APP_ZIP_CODE", "01234"), ("OTHER", "1")] (prefixed "APP_")
        extract l @?= Right (Cfg "x" 9000 5 "01234")
    , testCase "REGRESSION H2: underscores stay in keys unless split is asked for" $ do
        l <- ok [("APP_MAX_CONNECTIONS", "5")] (prefixed "APP_")
        node "max_connections" l @?= Just (Num (Finite 5))
        l2 <- ok [("APP_DB__HOST", "h"), ("APP_DB__PORT", "5432")] (prefixed "APP_" & Env.split "__")
        extract l2 @?= Right (Outer (Db "h" 5432))
    , testCase "prefix is case-insensitive, keys are lower-cased" $ do
        l <- ok [("app_Host", "x")] (prefixed "APP_")
        node "host" l @?= Just (String "x")
        l2 <- ok [("APP_Host", "x")] (prefixed "APP_" & Env.lowercase False)
        node "Host" l2 @?= Just (String "x")
    , testCase "structured values" $ do
        l <- ok [("APP_LIST", "[1, two, \"three\"]"), ("APP_MAP", "{a = 1, b = true}"), ("APP_Q", "\"quoted\"")] (prefixed "APP_")
        fmap stripTags (findValue (keyPath "list") l) @?= Just (V.array [V.number 1, V.string "two", V.string "three"])
        fmap stripTags (findValue (keyPath "map") l) @?= Just (V.dict [("a", V.number 1), ("b", V.bool True)])
        node "q" l @?= Just (String "quoted")
    , testCase "malformed structured text is just a string" $ do
        l <- ok [("APP_X", "[1, 2")] (prefixed "APP_")
        node "x" l @?= Just (String "[1, 2")
    , testCase "conflicts are reported, not silently resolved" $ do
        r <- env [("APP_DB", "x"), ("APP_DB__HOST", "y")] (prefixed "APP_" & Env.split "__")
        case r of
          Left es -> map errorKind (errorList es) @?= [KeyConflict "APP_DB" "APP_DB__HOST"]
          Right _ -> assertFailure "expected a conflict"
        r2 <- env [("APP_HOST", "a"), ("APP_host", "b")] (prefixed "APP_")
        case r2 of
          Left es -> map errorKind (errorList es) @?= [DuplicateKey "APP_HOST and APP_host"]
          Right _ -> assertFailure "expected a duplicate"
    , testCase "only / ignore / map / filter / ignoreEmpty" $ do
        let vars = [("APP_A", "1"), ("APP_B", "2"), ("APP_C", ""), ("APP_D", "4")]
        l <- ok vars (prefixed "APP_" & Env.only ["a", "C"])
        Map.keys <$> (extract l :: Either ConfigErrors (Map.Map Text Value)) @?= Right ["a", "c"]
        l2 <- ok vars (prefixed "APP_" & Env.ignore ["a"] & Env.ignoreEmpty True)
        Map.keys <$> (extract l2 :: Either ConfigErrors (Map.Map Text Value)) @?= Right ["b", "d"]
        l3 <- ok vars (prefixed "APP_" & Env.mapKeys (<> "_x") & Env.filterKeys (/= "D_x"))
        Map.keys <$> (extract l3 :: Either ConfigErrors (Map.Map Text Value)) @?= Right ["a_x", "b_x", "c_x"]
    , testCase "unparsed keeps strings" $ do
        l <- ok [("APP_N", "5")] (prefixed "APP_" & Env.unparsed)
        node "n" l @?= Just (String "5")
    , testCase "empty key segments are ignored" $ do
        l <- ok [("APP_", "1"), ("APP_A____B", "2"), ("APP_OK", "3")] (prefixed "APP_" & Env.split "__")
        Map.keys <$> (extract l :: Either ConfigErrors (Map.Map Text Value)) @?= Right ["ok"]
    , testCase "profiles and global" $ do
        l <- ok [("APP_PORT", "1")] (prefixed "APP_" & Env.inProfile "prod")
        profiles l @?= ["prod"]
        l2 <- ok [("APP_PORT", "1")] (prefixed "APP_" & Env.global)
        profiles l2 @?= [Global]
    , testCase "error messages name the variable" $ do
        l <- ok [("APP_DB__PORT", "abc"), ("APP_DB__HOST", "h")] (prefixed "APP_" & Env.split "__")
        case extract l :: Either ConfigErrors Outer of
          Left es -> assertBool (Text.unpack (renderErrors es)) ("`APP_DB__PORT`" `Text.isInfixOf` renderErrors es)
          Right v -> assertFailure (show v)
    , testCase "reads the real process environment" $ Jail.withJail $ \j -> do
        Jail.clearEnv j
        Jail.setEnv j "JAILED_HOST" "real"
        l <- load (from (prefixed "JAILED_")) >>= either (assertFailure . show) pure
        node "host" l @?= Just (String "real")
    ]

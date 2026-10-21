module Figment.EnvProvider (tests) where

import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Provider.Env (parseEnvKey)

tests :: TestTree
tests = testGroup "EnvProvider"
  [ testCase "simple single-key" $
      parseEnvKey "APP_" "APP_HOST"
        @?= Just [Text.pack "host"]
  , testCase "dotted path" $
      parseEnvKey "APP_" "APP_DB_HOST"
        @?= Just [Text.pack "db", Text.pack "host"]
  , testCase "case folding (RT-03)" $
      parseEnvKey "APP_" "APP_DB_HOST"
        @?= parseEnvKey "APP_" "APP_db_host"
  , testCase "prefix-only rejected" $
      parseEnvKey "APP_" "APP_"
        @?= Nothing
  , testCase "no-prefix rejected" $
      parseEnvKey "APP_" "OTHER_HOST"
        @?= Nothing
  , testCase "leading underscore dropped" $
      parseEnvKey "APP_" "APP__HOST"
        @?= Just [Text.pack "host"]
  ]

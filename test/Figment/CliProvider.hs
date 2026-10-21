module Figment.CliProvider (tests) where

import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Provider.Cli (parseCliArg)

tests :: TestTree
tests = testGroup "CliProvider"
  [ testCase "simple" $
      parseCliArg "--host=localhost"
        @?= Just (Text.pack "host", Text.pack "localhost")
  , testCase "dotted key" $
      parseCliArg "--db.host=localhost"
        @?= Just (Text.pack "db.host", Text.pack "localhost")
  , testCase "case folded" $
      parseCliArg "--DB.HOST=x"
        @?= Just (Text.pack "db.host", Text.pack "x")
  , testCase "no equals rejected" $
      parseCliArg "--host" @?= Nothing
  , testCase "no dashes rejected" $
      parseCliArg "host=x" @?= Nothing
  ]

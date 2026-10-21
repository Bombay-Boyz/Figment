module Figment.JsonProvider (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Provider.Json (fromAeson)
import Figment.Value

tests :: TestTree
tests = testGroup "JsonProvider"
  [ testCase "fromAeson scalar" $
      fromAeson (Aeson.Number 42) @?= RawNum 42
  , testCase "fromAeson object" $
      fromAeson (Aeson.object [("k", Aeson.String "v")])
        @?= RawObj [(Text.pack "k", RawStr "v")]
  , testCase "fromAeson array" $
      fromAeson (Aeson.toJSON [1 :: Int, 2])
        @?= RawArr [RawNum 1, RawNum 2]
  ]

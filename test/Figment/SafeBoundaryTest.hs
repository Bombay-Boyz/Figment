-- | Trustworthy test harness wrapping the Safe probe (ADR 014).
module Figment.SafeBoundaryTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Figment.SafeBoundary (safeBoundaryProbe)

tests :: TestTree
tests = testGroup "SafeBoundary"
  [ testCase "pure core importable under Safe" $
      safeBoundaryProbe @?= True
  ]

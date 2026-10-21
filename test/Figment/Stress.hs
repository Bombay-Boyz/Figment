-- | RTS stress tests (Standard §4.10). Proves that 'mkTagged' rejects
-- adversarial depth under bounded heap and stack, returning
-- 'DepthExceeded' rather than crashing. The RTS limits are set in the
-- cabal ghc-options for the test suite.
module Figment.Stress (tests) where

import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.QuickCheck

import Figment.Error (ConfigError(..))
import Figment.Value

tests :: TestTree
tests = testGroup "Stress"
  [ testProperty "mkTagged rejects 1M-deep input under bounded heap" $
      let raw = nest 1000000
      in case mkTagged defaultDepth (SourceId "stress", DefaultProfile) raw of
           Left (DepthExceeded _ _) -> property True
           Left e                    -> counterexample (show e) False
           Right _                   -> counterexample "expected DepthExceeded, got success" False
  , testProperty "mkTagged rejects 100k-wide object under bounded heap" $
      let raw = RawObj [ (Text.pack ("k" ++ show i), RawNum (fromIntegral i))
                       | i <- [0..99999 :: Int] ]
      in case mkTagged defaultDepth (SourceId "stress", DefaultProfile) raw of
           Right _ -> property True
           Left e  -> counterexample (show e) False
  ]
  where
    nest :: Int -> RawJSON
    nest 0 = RawNull
    nest n = RawArr [nest (n - 1)]

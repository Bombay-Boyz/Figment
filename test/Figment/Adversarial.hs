-- | Adversarial and stress properties (Standard §5.4, §2.4).
module Figment.Adversarial (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.QuickCheck

import Figment.Error (ConfigError(..))
import Figment.Value
import Figment.Merge
import Figment.Layer

tests :: TestTree
tests = testGroup "Adversarial"
  [ testProperty "mkTagged total on empty RawJSON" $
      property $ case mkTagged defaultDepth (SourceId "t", DefaultProfile) (RawObj []) of
        Right _ -> True
        Left _  -> False
  , testProperty "mkTagged rejects depth exactly budget+1" $
      forAll (chooseInt (1, 20)) $ \b ->
        case mkDepth b of
          Left _ -> property True
          Right d ->
            let raw = nest (b + 1)
            in case mkTagged d (SourceId "t", DefaultProfile) raw of
                 Left (DepthExceeded _ d') -> d' === d
                 _ -> counterexample "expected DepthExceeded" False
  , testProperty "mergeAlg total on all scalar pairs" $
      property $ and
        [ case mergeAlg bias ap a b of
            Tagged _ _ -> True
        | bias <- [LeftWins, RightWins]
        , ap   <- [Replace, Concat]
        , a    <- scalarTags
        , b    <- scalarTags
        ]
  , testProperty "runLayers never throws on empty chain" $
      property $ runLayers ([] :: [Layer]) === Tagged (VObj Map.empty) Merged
  , testProperty "mkTagged accepts a flat object of 1000 keys" $
      let raw = RawObj [ (Text.pack ("k" ++ show i), RawNum (fromIntegral i))
                       | i <- [0..999 :: Int] ]
      in case mkTagged defaultDepth (SourceId "t", DefaultProfile) raw of
           Right _ -> property True
           Left e  -> counterexample ("unexpected: " ++ show e) False
  ]
  where
    nest :: Int -> RawJSON
    nest 0 = RawNull
    nest n = RawArr [nest (n - 1)]

    scalarTags :: [Tagged]
    scalarTags = map (\v -> Tagged v Merged)
      [ VNull, VBool True, VBool False, VNum 1, VNum 0, VStr "a", VStr "" ]

-- | Model-based test for runLayers (Standard §2.9).
module Figment.Model (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog

import Figment.Value
import Figment.Layer

data Op = SetKey String Int
  deriving (Eq, Show)

genOp :: Gen Op
genOp = SetKey <$> Gen.element ["a", "b", "c"] <*> Gen.int (Range.linear 0 10)

keyOf :: String -> Key
keyOf = Key . Text.pack

applyModel :: [Op] -> Map.Map Key Tagged
applyModel = foldl' step Map.empty
  where
    step m (SetKey k v) =
      Map.insert (keyOf k) (Tagged (VNum (fromIntegral v)) Merged) m

toLayer :: Op -> Layer
toLayer (SetKey k v) =
  Merge (Tagged (VObj (Map.singleton (keyOf k) (Tagged (VNum (fromIntegral v)) Merged))) Merged)

prop_runLayers_matches_model :: Property
prop_runLayers_matches_model = property $ do
  ops <- forAll $ Gen.list (Range.linear 0 20) genOp
  let Tagged result _ = runLayers (map toLayer ops)
  case result of
    VObj m -> Map.toList m === Map.toList (applyModel ops)
    _      -> failure

tests :: TestTree
tests = testGroup "Model"
  [ testProperty "runLayers matches ordered model" prop_runLayers_matches_model
  ]

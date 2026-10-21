-- | RT-04 (feasibility study §10): a round-trip through mkTagged →
-- merge → runExtract must preserve an arbitrary Scientific exactly.
module Figment.Precision (tests) where

import Data.Aeson (FromJSON)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Error (ConfigError)
import Figment.Extract (runExtract)
import Figment.Value

data Rec = Rec
  { n :: Scientific
  } deriving (Eq, Show, Generic)

instance FromJSON Rec

tests :: TestTree
tests = testGroup "Precision"
  [ testCase "scientific preserved through extraction" $ do
      let values = [1.5, 0.1, 123456789.123456789, 1e300, 1e-300]
      mapM_ checkOne values
  ]
  where
    checkOne :: Scientific -> Assertion
    checkOne v =
      let t = Tagged
                (VObj (Map.fromList
                  [ (Key "n", Tagged (VNum v) Merged) ]))
                Merged
      in case runExtract t :: Either ConfigError Rec of
           Right r -> n r @?= v
           Left e  -> assertFailure ("unexpected error: " ++ show e)

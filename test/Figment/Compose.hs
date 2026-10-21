module Figment.Compose (tests) where

import Data.Aeson (FromJSON)
import qualified Data.Text
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Figment

data Cfg = Cfg
  { host :: String
  , port :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Cfg

layerWith :: String -> Int -> Layer
layerWith h p =
  Merge (Tagged
    (VObj (Map.fromList
      [ (Key "host", Tagged (VStr (fromString h)) Merged)
      , (Key "port", Tagged (VNum (fromIntegral p)) Merged)
      ]))
    Merged)
  where
    fromString = Data.Text.pack

tests :: TestTree
tests = testGroup "Compose"
  [ testCase "composeUnchecked single layer" $
      let r = composeUnchecked [layerWith "localhost" 8080] :: Either ConfigError Cfg
      in r @?= Right (Cfg "localhost" 8080)
  , testCase "composeUnchecked layer merge (last wins on scalars)" $
      let r = composeUnchecked
                [ layerWith "a" 1
                , layerWith "b" 2
                ] :: Either ConfigError Cfg
      in r @?= Right (Cfg "b" 2)
  ]

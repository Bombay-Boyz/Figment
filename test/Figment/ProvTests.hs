module Figment.ProvTests (tests) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Error (ConfigError)
import Figment.Prov
import Figment.Value

data Cfg = Cfg { host :: String, port :: Int }
  deriving (Eq, Show, Generic)
instance FromJSON Cfg

mkLeaf :: (SourceId, Profile, [Key]) -> Value -> Tagged
mkLeaf (s, p, ks) v = Tagged v (Leaf s p (Path ks))

tests :: TestTree
tests = testGroup "Prov"
  [ testCase "collects per-leaf provenance by path" $ do
      let t = Tagged
                (VObj (Map.fromList
                  [ (Key "host", mkLeaf (SourceId "t", DefaultProfile, [Key "host"]) (VStr "localhost"))
                  , (Key "port", mkLeaf (SourceId "t", DefaultProfile, [Key "port"]) (VNum 8080))
                  ]))
                Merged
      case runExtractProv t :: Either ConfigError (Cfg, ProvMap) of
        Left e -> assertFailure ("unexpected: " ++ show e)
        Right (cfg, pm) -> do
          cfg @?= Cfg "localhost" 8080
          assertBool "host path present" $
            Map.member (Path [Key "host"]) pm
          assertBool "port path present" $
            Map.member (Path [Key "port"]) pm
  , testCase "nested arrays get indexed paths" $ do
      let t = Tagged
                (VObj (Map.fromList
                  [ (Key "xs", Tagged
                       (VArr [ Tagged (VNum 1) (Leaf (SourceId "t") DefaultProfile (Path [Key "xs", Key "0"]))
                              , Tagged (VNum 2) (Leaf (SourceId "t") DefaultProfile (Path [Key "xs", Key "1"]))
                              ])
                       Merged)
                  ]))
                Merged
      let pm = case runExtractProv t :: Either ConfigError (Rec, ProvMap) of
                 Right (_, m) -> m
                 Left _       -> Map.empty
      assertBool "xs[0] present" $
        Map.member (Path [Key "xs", Key "0"]) pm
      assertBool "xs[1] present" $
        Map.member (Path [Key "xs", Key "1"]) pm
  ]

data Rec = Rec { xs :: [Int] }
  deriving (Eq, Show, Generic)
instance FromJSON Rec

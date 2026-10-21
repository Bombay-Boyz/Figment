module Figment.ExtractTests (tests) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Extract (runExtract)
import Figment.Error (ConfigError(..))
import Figment.Value

data Simple = Simple
  { host :: String
  , port :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Simple

tests :: TestTree
tests = testGroup "Extract"
  [ testCase "decodes flat object" $ do
      let t = Tagged
                (VObj (Map.fromList
                  [ (Key "host", Tagged (VStr "localhost") Merged)
                  , (Key "port", Tagged (VNum 8080)        Merged)
                  ]))
                Merged
      runExtract t @?= Right (Simple "localhost" 8080)
  , testCase "decode failure yields DecodeError" $ do
      let t = Tagged
                (VObj (Map.fromList
                  [ (Key "host", Tagged (VStr "localhost") Merged)
                  , (Key "port", Tagged (VStr "not-a-number") Merged)
                  ]))
                Merged
      case runExtract t :: Either ConfigError Simple of
        Left (DecodeError _ _) -> pure ()
        other                  -> assertFailure ("expected DecodeError, got " ++ show other)
  ]

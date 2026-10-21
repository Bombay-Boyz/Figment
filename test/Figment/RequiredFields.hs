-- | Documents and tests the required/defaulted-field discipline (ADR
-- 013): missing required fields yield 'MissingKey'/'DecodeError' from
-- extraction; missing optional fields fall back to 'Nothing'.
module Figment.RequiredFields (tests) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Error (ConfigError(..))
import Figment.Extract (runExtract)
import Figment.Value

data Rec = Rec
  { host     :: String
  , maybePort :: Maybe Int
  } deriving (Eq, Show, Generic)
instance FromJSON Rec

tests :: TestTree
tests = testGroup "RequiredFields"
  [ testCase "missing required field yields DecodeError" $
      let t = Tagged (VObj (Map.fromList
                  [ (Key "maybePort", Tagged (VNum 8080) Merged) ]))
                Merged
      in case runExtract t :: Either ConfigError Rec of
           Left (DecodeError _ _) -> pure ()
           Left other             -> assertFailure ("expected DecodeError, got " ++ show other)
           Right r                -> assertFailure ("expected failure, got " ++ show r)
  , testCase "missing optional field decodes as Nothing" $
      let t = Tagged (VObj (Map.fromList
                  [ (Key "host", Tagged (VStr "localhost") Merged) ]))
                Merged
      in runExtract t @?= Right (Rec "localhost" Nothing)
  , testCase "both fields present decodes fully" $
      let t = Tagged (VObj (Map.fromList
                  [ (Key "host", Tagged (VStr "localhost") Merged)
                  , (Key "maybePort", Tagged (VNum 8080) Merged) ]))
                Merged
      in runExtract t @?= Right (Rec "localhost" (Just 8080))
  ]

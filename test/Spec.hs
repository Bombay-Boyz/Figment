module Main (main) where

import Test.Tasty

import qualified Figment.CompileFail as CF
import qualified Figment.ExtractTests as E
import qualified Figment.Properties as P

main :: IO ()
main = defaultMain $ testGroup "figment"
  [ P.tests
  , E.tests
  , CF.tests
  ]

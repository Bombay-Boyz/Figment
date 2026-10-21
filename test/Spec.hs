module Main (main) where

import Test.Tasty

import qualified Figment.CompileFail as CF
import qualified Figment.ExtractTests as E
import qualified Figment.CliProvider as CL
import qualified Figment.EnvProvider as EP
import qualified Figment.JsonProvider as J
import qualified Figment.Properties as P

main :: IO ()
main = defaultMain $ testGroup "figment"
  [ P.tests
  , E.tests
  , J.tests
  , EP.tests
  , CL.tests
  , CF.tests
  ]

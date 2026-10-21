module Main (main) where

import Test.Tasty

import qualified Figment.CompileFail as CF
import qualified Figment.RequiredFields as RF
import qualified Figment.FileProviders as FP
import qualified Figment.Compose as CO
import qualified Figment.ExtractTests as E
import qualified Figment.CliProvider as CL
import qualified Figment.EnvProvider as EP
import qualified Figment.JsonProvider as J
import qualified Figment.Precision as PR
import qualified Figment.ProvTests as PV
import qualified Figment.Properties as P

main :: IO ()
main = defaultMain $ testGroup "figment"
  [ P.tests
  , E.tests
  , J.tests
  , PR.tests
  , PV.tests
  , EP.tests
  , CL.tests
  , CO.tests
  , RF.tests
  , FP.tests
  , CF.tests
  ]

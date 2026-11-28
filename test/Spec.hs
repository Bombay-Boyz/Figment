module Main (main) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty

import qualified Figment.Test.Adversarial as Adversarial
import qualified Figment.Test.AuditFixes as AuditFixes
import qualified Figment.Test.Cli as Cli
import qualified Figment.Test.Decode as Decode
import qualified Figment.Test.Env as Env
import qualified Figment.Test.Errors as Errors
import qualified Figment.Test.Figment as Figment
import qualified Figment.Test.Hardening as Hardening
import qualified Figment.Test.MergeLaws as MergeLaws
import qualified Figment.Test.Partial as Partial
import qualified Figment.Test.Providers as Providers
import qualified Figment.Test.Readme as Readme
import qualified Figment.Test.Tutorial as Tutorial

main :: IO ()
main = do
  -- resolved before any test runs: Jail changes the working directory
  goldenDir <- (</> ("test" </> "golden")) <$> getCurrentDirectory
  defaultMain $
    testGroup
      "figment"
      [ MergeLaws.tests
      , Figment.tests
      , Decode.tests
      , Partial.tests
      , Providers.tests
      , Env.tests
      , Cli.tests
      , Errors.tests goldenDir
      , Adversarial.tests
      , Hardening.tests
      , AuditFixes.tests
      , Readme.tests
      , Tutorial.tests
      ]

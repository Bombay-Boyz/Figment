module Figment.CompileFail (tests) where

import Data.List (isInfixOf)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CompileFail"
  [ testCase "NoProvider fails with §6.2 message" $ do
      (code, _, err) <- ghcNoCode "test/CompileFail/NoProvider.hs"
      assertEqual "must not compile" (ExitFailure 1) code
      assertBool ("stderr must name the cause; got: " ++ err) $
        "Figment: no provider" `isInfixOf` err
  , testCase "OkProvider compiles" $ do
      (code, _, err) <- ghcNoCode "test/CompileFail/OkProvider.hs"
      assertEqual ("must compile; stderr: " ++ err) ExitSuccess code
  ]

ghcNoCode :: FilePath -> IO (ExitCode, String, String)
ghcNoCode path = readProcessWithExitCode
  "cabal"
  [ "exec", "--", "ghc", "-package", "figment", "-fno-code", path ]
  ""

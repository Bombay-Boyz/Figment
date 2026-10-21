module Main (main) where

import Criterion.Main

import qualified Figment.Bench as B

main :: IO ()
main = defaultMain B.benchmarks

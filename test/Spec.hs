module Main (main) where

import Test.Tasty

import qualified Figment.Properties as P

main :: IO ()
main = defaultMain P.tests

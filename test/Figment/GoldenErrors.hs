-- | Golden tests for the rendered form of 'ConfigError' (Standard §2.15).
-- These pin the public message format; a change here is a breaking
-- change to the library's observable behaviour and requires a version
-- bump (Standard §2.13).
module Figment.GoldenErrors (tests) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.Golden

import Figment.Error (ConfigError(..))
import Figment.Value (Key(..), Path(..), defaultDepth)

render :: ConfigError -> Text
render e = Text.pack (show e)

cases :: [(FilePath, ConfigError)]
cases =
  [ ("test/golden/err-source.txt",
      SourceError "config.toml" "unexpected token at line 3")
  , ("test/golden/err-depth.txt",
      DepthExceeded (Path [Key (Text.pack "a"), Key (Text.pack "b"), Key (Text.pack "3")]) defaultDepth)
  , ("test/golden/err-decode.txt",
      DecodeError (Path [Key (Text.pack "port")]) "expected Int, got String")
  , ("test/golden/err-missing.txt",
      MissingKey (Path [Key (Text.pack "host")]))
  , ("test/golden/err-validation.txt",
      ValidationError (Path []) "port must be in 1..65535")
  , ("test/golden/err-configuration.txt",
      ConfigurationError "Depth" "must be a positive Int")
  ]

tests :: TestTree
tests = testGroup "GoldenErrors"
  [ goldenVsString name path (pure (BL.fromStrict (TE.encodeUtf8 (render e))))
  | (path, e) <- cases
  , let name = takeFileName path
  ]
  where
    takeFileName = reverse . takeWhile (/= '/') . reverse

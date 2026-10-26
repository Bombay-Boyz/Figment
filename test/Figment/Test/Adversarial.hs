-- | Hostile input: limits are enforced inside the parsers, nothing
-- escapes as an exception.
module Figment.Test.Adversarial (tests) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Figment
import qualified Figment.Provider.Env as Env

kinds :: Figment -> IO [ErrorKind]
kinds f = either (map errorKind . errorList) (const []) <$> load f

deepArrays :: Int -> Text
deepArrays n = Text.replicate n "[" <> Text.replicate n "]"

limits :: Int -> Int -> Limits
limits d n = either (const defaultLimits) id (mkLimits d n)

tests :: TestTree
tests =
  testGroup
    "adversarial"
    [ testCase "1M-deep JSON is rejected at the depth limit, quickly" $ do
        k <- kinds (from (fromText json ("{\"a\":" <> deepArrays 1000000 <> "}")))
        k @?= [DepthExceeded 64]
    , testCase "1M-deep YAML flow sequence is rejected" $ do
        k <- kinds (from (fromText yaml ("a: " <> deepArrays 100000 <> "\n")))
        k @?= [DepthExceeded 64]
    , testCase "deep TOML inline arrays are rejected" $ do
        k <- kinds (from (fromText toml ("a = " <> deepArrays 10000 <> "\n")))
        k @?= [DepthExceeded 64]
    , testCase "deep structured env values are rejected" $ do
        k <- kinds (from (Env.fromList [("APP_X", deepArrays 10000)] (prefixed "APP_")))
        k @?= [DepthExceeded 64]
    , testCase "depth limit is exact: 64 accepted, 65 rejected" $ do
        let nestedJson n = Text.replicate n "{\"a\":" <> "1" <> Text.replicate n "}"
        ok <- kinds (from (fromText json (nestedJson 64)))
        ok @?= []
        bad <- kinds (from (fromText json (nestedJson 65)))
        bad @?= [DepthExceeded 64]
    , testCase "billion laughs: alias expansion is charged to the node limit" $ do
        let lol = Text.unlines $
              "a: &a [x, x, x, x, x, x, x, x, x, x]"
                : [ Text.pack (c : ": &" ++ [c] ++ " [" ++ concat (replicate 10 ('*' : [p] ++ ", ")) ++ "*" ++ [p] ++ "]")
                  | (c, p) <- zip "bcdefgh" "abcdefg" ]
        k <- kinds (withLimits (limits 64 100000) (from (fromText yaml lol)))
        k @?= [TooManyNodes 100000]
    , testCase "huge exponents do not allocate huge integers" $ do
        l <- load (from (fromText json "{\"n\":1e1000000000}")) >>= either (assertFailure . show) pure
        case extractAt (keyPath "n") l :: Either ConfigErrors Integer of
          Left es -> map errorKind (errorList es) @?= [InvalidValue "number 1.0e1000000000" "an integer with at most ~4096 digits"]
          Right _ -> assertFailure "accepted"
        case extractAt (keyPath "n") l :: Either ConfigErrors Int of
          Left _ -> pure ()
          Right v -> assertFailure (show v)
    , testCase "custom limits are validated" $
        case mkLimits 0 1 of
          Left e -> errorKind e @?= InvalidArgument "maxDepth must be positive, got 0"
          Right _ -> assertFailure "accepted"
    , testProperty "no format throws on arbitrary bytes" $ \(bytes :: [Word8]) ->
        ioProperty $ do
          let bs = BS.pack bytes
              txt = TE.decodeUtf8Lenient bs
          rs <- mapM (\fmt -> try (load (from (fromText fmt txt)) >>= evaluate . force . either (Left . renderErrors) (Right . show . selected)))
                     [json, toml, yaml]
          pure $ conjoin [counterexample (show e) False | Left (e :: SomeException) <- rs]
    , testProperty "no format throws on near-miss documents" $
        forAll (elements seeds) $ \seed -> forAll (chooseInt (0, Text.length seed)) $ \cut ->
          ioProperty $ do
            rs <- mapM (\fmt -> try (load (from (fromText fmt (Text.take cut seed))) >>= evaluate . force . either (Left . renderErrors) (Right . show . selected)))
                       [json, toml, yaml]
            pure $ conjoin [counterexample (show e) False | Left (e :: SomeException) <- rs]
    ]
  where
    seeds =
      [ "{\"a\": [1, 2, {\"b\": \"c\\u00e9\"}], \"d\": null}"
      , "[t]\na = 1\nb = [1, 2]\nc = { d = \"e\" }\n[[u]]\nx = 2021-01-01\n"
      , "a: &x [1, 2]\nb: *x\nc:\n  <<: {k: v}\n  d: 'q'\n"
      ]


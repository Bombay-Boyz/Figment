{-# LANGUAGE ScopedTypeVariables #-}
-- | Parser fuzz properties (Standard §5.4, §2.4). Throws arbitrary
-- bytes at each file provider and asserts that no exception escapes:
-- the provider returns 'Left SourceError' on malformed input, never
-- crashes.
module Figment.ParserFuzz (tests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QC

import Figment.Error (ConfigError(..))
import Figment.Provider (tagProviderDefault)
import Figment.Provider.Json (JsonFile(..))
import Figment.Provider.Toml (TomlFile(..))
import Figment.Provider.Yaml (YamlFile(..))
import Figment.Value (Tagged)

-- | Run a provider on arbitrary bytes written to a temp file, and
-- assert no exception escapes.
newtype ArbBytes = ArbBytes ByteString
  deriving (Show)

instance Arbitrary ArbBytes where
  arbitrary = ArbBytes . BS.pack <$> QC.arbitrary

prop_no_exception_escapes
  :: (FilePath -> IO (Either ConfigError Tagged)) -- ^ provider runner
  -> String                                        -- ^ extension
  -> ArbBytes                                      -- ^ arbitrary bytes
  -> Property
prop_no_exception_escapes run ext (ArbBytes bytes) = ioProperty $
  withSystemTempDirectory "figment-fuzz" $ \dir -> do
    let path = dir </> ("fuzz" ++ ext)
    BS.writeFile path bytes
    outcome <- try (evaluate =<< run path)
    case outcome of
      Left (e :: SomeException) ->
        pure $ counterexample
          ("exception escaped: " ++ show e ++ "\ninput bytes: " ++ show bytes)
          False
      Right _ -> pure (property True)

tests :: TestTree
tests = testGroup "ParserFuzz"
  [ testProperty "JsonFile never throws on arbitrary bytes" $
      prop_no_exception_escapes
        (\p -> tagProviderDefault (JsonFile p)) ".json"
  , testProperty "TomlFile never throws on arbitrary bytes" $
      prop_no_exception_escapes
        (\p -> tagProviderDefault (TomlFile p)) ".toml"
  , testProperty "YamlFile never throws on arbitrary bytes" $
      prop_no_exception_escapes
        (\p -> tagProviderDefault (YamlFile p)) ".yaml"
  ]

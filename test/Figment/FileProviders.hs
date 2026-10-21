-- | End-to-end tests for the file-based providers: write a temp file,
-- run 'tagProvider', merge, and decode with 'runExtract'.
module Figment.FileProviders (tests) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty
import Test.Tasty.HUnit

import Figment.Extract (runExtract)
import Figment.Provider (tagProviderDefault)
import Figment.Provider.Json (JsonFile(..))
import Figment.Provider.Toml (TomlFile(..))
import Figment.Provider.Yaml (YamlFile(..))

data Cfg = Cfg
  { host :: String
  , port :: Int
  } deriving (Eq, Show, Generic)
instance FromJSON Cfg

writeFile' :: FilePath -> String -> IO ()
writeFile' path content = writeFile path content

withTempFile :: String -> String -> (FilePath -> IO a) -> IO a
withTempFile ext content action =
  withSystemTempDirectory "figment-test" $ \dir -> do
    let path = dir </> ("cfg" ++ ext)
    writeFile' path content
    action path

tests :: TestTree
tests = testGroup "FileProviders"
  [ testCase "JsonFile round-trip" $
      withTempFile ".json" "{\"host\":\"a\",\"port\":1}" $ \path -> do
        r <- tagProviderDefault (JsonFile path)
        case r of
          Left e  -> assertFailure ("tag failed: " ++ show e)
          Right t -> runExtract t @?= Right (Cfg "a" 1)
  , testCase "TomlFile round-trip" $
      withTempFile ".toml" "host = \"b\"\nport = 2\n" $ \path -> do
        r <- tagProviderDefault (TomlFile path)
        case r of
          Left e  -> assertFailure ("tag failed: " ++ show e)
          Right t -> runExtract t @?= Right (Cfg "b" 2)
  , testCase "YamlFile round-trip" $
      withTempFile ".yaml" "host: c\nport: 3\n" $ \path -> do
        r <- tagProviderDefault (YamlFile path)
        case r of
          Left e  -> assertFailure ("tag failed: " ++ show e)
          Right t -> runExtract t @?= Right (Cfg "c" 3)
  ]

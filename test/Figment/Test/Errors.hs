-- | Golden tests for rendered error reports. The rendering is part of
-- the public interface (users read it); changing it is a visible change.
module Figment.Test.Errors (tests) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Word (Word16)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)

import Figment
import qualified Figment.Provider.Env as Env

data Db = Db {host :: Text, port :: Word16}
  deriving stock (Show, Generic)

instance FromConfig Db where
  fromConfig = genericDecoder defaultOptions {rejectUnknownFields = True}

data Cfg = Cfg {name :: Text, db :: Db, level :: Level}
  deriving stock (Show, Generic)
  deriving anyclass (FromConfig)

data Level = Debug | Info
  deriving stock (Show, Generic)
  deriving anyclass (FromConfig)

report :: Figment -> IO Text
report f = do
  r <- load f
  pure $ case r of
    Left es -> renderErrors es
    Right l -> case extract l :: Either ConfigErrors Cfg of
      Left es -> renderErrors es
      Right v -> Text.pack ("unexpected success: " ++ show v)

-- | @golden dir name action@: the golden files live in @dir@, which must be
-- absolute. 'Jail' changes the process working directory while a jail test
-- runs and the suite runs its tests in parallel, so a relative path would be
-- looked up in the wrong directory whenever the two overlap.
golden :: FilePath -> String -> IO Text -> TestTree
golden dir n act = goldenTest n (BL.readFile path) (BL.fromStrict . TE.encodeUtf8 . (<> "\n") <$> act) cmp (BL.writeFile path)
  where
    path = dir </> (n ++ ".txt")
    -- compare ignoring carriage returns, so a Windows checkout that
    -- converted the golden file to CRLF still matches
    norm = BL.filter (/= 13)
    cmp expected actual
      | norm expected == norm actual = pure Nothing
      | otherwise = pure (Just ("actual output:\n" ++ BLC.unpack actual))

-- | The argument is the absolute path of the @test/golden@ directory.
tests :: FilePath -> TestTree
tests dir =
  testGroup
    "errors (golden)"
    [ golden dir "decode-many" $
        report $
          new
            & merge (fromText toml "[db]\nhost = \"h\"\nport = 70000\nhots = 1\n")
            & merge (Env.fromList [("APP_LEVEL", "verbose")] (prefixed "APP_"))
    , golden dir "missing" $
        report $
          new
            & merge (fromText json "{\"db\":{\"host\":\"h\",\"port\":1},\"level\":\"Info\"}")
            & merge (Env.fromList [] (prefixed "APP_") & Env.split "__")
            & merge (cli [])
    , golden dir "load-failures" $
        report $
          new
            & merge (fromText yaml "a: 1\na: 2\n")
            & merge (fromText toml "x = \n")
            & merge (Env.fromList [("APP_DB", "1"), ("APP_DB__HOST", "2")] (prefixed "APP_" & Env.split "__"))
    , golden dir "single" $
        report (new & merge (fromText json "{\"name\":\"n\",\"db\":{\"host\":\"h\",\"port\":\"x\"},\"level\":\"Debug\"}"))
    ]

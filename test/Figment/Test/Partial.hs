{-# LANGUAGE TypeFamilies #-}

-- | Higher-kinded partial configurations.
module Figment.Test.Partial (tests) where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import qualified Figment.Provider.Env as Env

data DbF f = Db
  { url :: Field f Text
  , pool :: Field f Int
  }
  deriving stock (Generic)

instance Partial DbF

data AppF f = App
  { host :: Field f Text
  , port :: Field f Int
  , motd :: Field f (Maybe Text)
  , db :: DbF f
  }
  deriving stock (Generic)

instance Partial AppF

deriving stock instance Eq (DbF Identity)
deriving stock instance Show (DbF Identity)
deriving stock instance Eq (AppF Identity)
deriving stock instance Show (AppF Identity)

type App = AppF Identity

partialDefaults :: AppF Maybe
partialDefaults = (emptyPartial @AppF) {port = Just 8080, db = (emptyPartial @DbF) {pool = Just 4}}

totalDefaults :: App
totalDefaults = App "localhost" 8080 Nothing (Db "sqlite::memory:" 4)

loaded :: [(Text, Text)] -> Text -> IO Loaded
loaded vars body =
  load (new & merge (fromText toml body) & merge (Env.fromList vars (prefixed "APP_") & Env.split "__"))
    >>= either (assertFailure . Text.unpack . renderErrors) pure

tests :: TestTree
tests =
  testGroup
    "partial (HKD)"
    [ testCase "defaults fill gaps; providers win; nested records" $ do
        l <- loaded [("APP_DB__URL", "postgres://x")] "host = \"h\"\n"
        extractPartial partialDefaults l @?= Right (App "h" 8080 Nothing (Db "postgres://x" 4))
    , testCase "every missing field is reported, with how to set it" $ do
        l <- loaded [] "port = 1\n"
        case extractPartial partialDefaults l of
          Left es -> do
            map errorKind (errorList es) @?= [MissingField "host", MissingField "url"]
            map (renderPath . errorPath) (errorList es) @?= ["host", "db.url"]
            let hints = Text.unlines (concatMap errorHints (errorList es))
            assertBool (Text.unpack hints) ("`APP_DB__URL`" `Text.isInfixOf` hints)
          Right v -> assertFailure (show v)
    , testCase "type errors in present fields are all reported" $ do
        l <- loaded [] "port = \"x\"\n[db]\npool = \"y\"\n"
        case extractPartial partialDefaults l of
          Left es -> map (renderPath . errorPath) (errorList es) @?= ["port", "db.pool"]
          Right v -> assertFailure (show v)
    , testCase "total defaults: missing fields are impossible" $ do
        l <- loaded [] ""
        extractTotal totalDefaults l @?= Right totalDefaults
        l2 <- loaded [("APP_PORT", "9")] "motd = \"hi\"\n"
        extractTotal totalDefaults l2 @?= Right totalDefaults {port = 9, motd = Just "hi"}
    , testCase "overlay is right-biased; total/complete round-trip" $ do
        let a = (emptyPartial @AppF) {host = Just "a", port = Just 1}
            b = (emptyPartial @AppF) {port = Just 2}
        fmap port (complete defaultOptions (overlay (total totalDefaults) (overlay a b))) @?= Right 2
        complete defaultOptions (total totalDefaults) @?= Right totalDefaults
    ]

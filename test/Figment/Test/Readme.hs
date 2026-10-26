{-# LANGUAGE TypeFamilies #-}

-- | The examples from README.md, compiled and run, so the README cannot
-- rot (it did in 0.1: its headline example did not compile).
module Figment.Test.Readme (tests) where

import Data.Function ((&))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Figment as F
import qualified Figment.Jail as Jail
import qualified Figment.Provider.Env as Env

-- README: quick start ---------------------------------------------------

data Config = Config
  { name :: Text
  , port :: Int
  , debug :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (F.FromConfig)

figment :: F.Figment
figment =
  F.new
    & F.merge (F.defaults (Aeson.object ["port" .= (8080 :: Int), "debug" .= False]))
    & F.merge (F.file F.toml "App.toml")
    & F.merge (F.prefixed "APP_")

-- README: partial configurations ------------------------------------------

data ServerF f = Server
  { host :: F.Field f Text
  , listen :: F.Field f Int
  , workers :: F.Field f Int
  }
  deriving stock (Generic)

instance F.Partial ServerF

deriving stock instance Show (ServerF F.Identity)
deriving stock instance Eq (ServerF F.Identity)

serverDefaults :: ServerF Maybe
serverDefaults = (F.emptyPartial @ServerF) {listen = Just 8080, workers = Just 4}

tests :: TestTree
tests =
  testGroup
    "README"
    [ testCase "quick start" $ Jail.withJail $ \j -> do
        _ <- Jail.createFile j "App.toml" "name = \"demo\"\n"
        Jail.setEnv j "APP_DEBUG" "true"
        cfg <- F.loadExtractOrThrow figment
        cfg @?= Config "demo" 8080 True
    , testCase "partial configurations" $ do
        l <- F.load (F.from (Env.fromList [("SRV_HOST", "0.0.0.0")] (F.prefixed "SRV_")))
               >>= either (assertFailure . Text.unpack . F.renderErrors) pure
        F.extractPartial serverDefaults l @?= Right (Server "0.0.0.0" 8080 4)
    ]

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

-- | Every code snippet in "Figment.Tutorial", compiled and run. If you
-- change one, change the other.
module Figment.Test.Tutorial (tests) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Figment as F
import qualified Figment.Jail as Jail
import qualified Figment.Provider.Env as Env

-- §2 Your first configuration ---------------------------------------------

data Config = Config
  { name :: Text
  , port :: Int
  , debug :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (F.FromConfig)

firstFigment :: F.Figment
firstFigment = F.from (F.file F.toml "App.toml")

-- §3 Layering sources --------------------------------------------------------

layered :: [Text] -> F.Figment
layered args =
  F.new
    & F.merge (F.defaults (Aeson.object ["port" .= (8080 :: Int), "debug" .= False]))
    & F.merge (F.file F.toml "App.toml")
    & F.merge (F.prefixed "APP_")
    & F.merge (F.cli args)

-- §4 Environment variables ---------------------------------------------------

data Db = Db {host :: Text, port :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (F.FromConfig)

newtype WithDb = WithDb {db :: Db}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (F.FromConfig)

dbFromEnv :: F.Figment
dbFromEnv = F.from (F.prefixed "APP_" & Env.split "__")

-- §6 Profiles ----------------------------------------------------------------

profiled :: F.Profile -> F.Figment
profiled p =
  F.new
    & F.merge (F.nested (F.file F.toml "App.toml"))
    & F.select p

-- §8 Decoding in depth -------------------------------------------------------

data Level = Debug | Info | Warning
  deriving stock (Show, Eq, Generic)
  deriving anyclass (F.FromConfig)

data Server = Server
  { listenAddress :: Text
  , maxConnections :: Int
  , logLevel :: Level
  }
  deriving stock (Show, Eq, Generic)

instance F.FromConfig Server where
  fromConfig =
    F.genericDecoder
      F.defaultOptions
        { F.fieldLabelModifier = F.snakeCase
        , F.rejectUnknownFields = True
        }

newtype Port = Port Int
  deriving stock (Show, Eq)

instance F.FromConfig Port where
  fromConfig = F.validate check F.fromConfig
    where
      check n
        | n > 0 && n < 65536 = Right (Port n)
        | otherwise = Left "a port must be between 1 and 65535"

data Endpoint = Endpoint {url :: Text, timeout :: Int, retries :: Int}
  deriving stock (Show, Eq)

instance F.FromConfig Endpoint where
  fromConfig =
    Endpoint
      <$> F.field "url"
      <*> F.fieldOr "timeout" 30
      <*> F.fieldOr "retries" 3

-- §9 Provenance --------------------------------------------------------------

data Paths = Paths
  { dataDir :: F.RelativePath
  , token :: F.Tagged Text
  }
  deriving stock (Show, Eq, Generic)

instance F.FromConfig Paths where
  fromConfig = F.genericDecoder F.defaultOptions {F.fieldLabelModifier = F.snakeCase}

-- §10 Partial configurations -------------------------------------------------

data AppF f = App
  { appName :: F.Field f Text
  , workers :: F.Field f Int
  , motd :: F.Field f (Maybe Text)
  }
  deriving stock (Generic)

instance F.Partial AppF

deriving stock instance Show (AppF F.Identity)
deriving stock instance Eq (AppF F.Identity)

type App = AppF F.Identity

appDefaults :: AppF Maybe
appDefaults = (F.emptyPartial @AppF) {workers = Just 4}

-- §11 For library authors -----------------------------------------------------

libraryFigment :: F.Figment
libraryFigment =
  F.new
    & F.merge (F.defaults (Aeson.object ["workers" .= (4 :: Int)]))
    & F.merge (F.prefixed "MYLIB_")

applicationFigment :: F.Figment
applicationFigment = libraryFigment <> F.from (F.file F.toml "MyApp.toml")

-- §12 Writing a provider ------------------------------------------------------

-- | Configuration held in a Haskell map, e.g. fetched from a secrets
-- service at startup.
newtype Secrets = Secrets (Map.Map Text Text)

instance F.Provider Secrets where
  metadata _ = F.named "secrets service"
  provide _limits (Secrets m) =
    pure (Right (Map.singleton F.Default (fmap (F.Value untaggedTag . F.String) m)))

untaggedTag :: F.Tag
untaggedTag = F.Tag Nothing Nothing Nothing Nothing

-- tests --------------------------------------------------------------------

ok :: F.Figment -> IO F.Loaded
ok f = F.load f >>= either (assertFailure . Text.unpack . F.renderErrors) pure

tests :: TestTree
tests =
  testGroup
    "tutorial"
    [ testCase "§2 first configuration" $ Jail.withJail $ \j -> do
        _ <- Jail.createFile j "App.toml" "name = \"demo\"\nport = 8080\ndebug = false\n"
        cfg <- F.loadExtractOrThrow firstFigment
        cfg @?= Config "demo" 8080 False
    , testCase "§3 layering" $ Jail.withJail $ \j -> do
        _ <- Jail.createFile j "App.toml" "name = \"demo\"\nport = 9000\n"
        Jail.setEnv j "APP_DEBUG" "true"
        cfg <- F.loadExtractOrThrow (layered ["--port=9100"])
        cfg @?= Config "demo" 9100 True
    , testCase "§4 env" $ Jail.withJail $ \j -> do
        Jail.setEnv j "APP_DB__HOST" "db.internal"
        Jail.setEnv j "APP_DB__PORT" "5432"
        cfg <- F.loadExtractOrThrow dbFromEnv
        cfg @?= WithDb (Db "db.internal" 5432)
    , testCase "§6 profiles" $ Jail.withJail $ \j -> do
        _ <- Jail.createFile j "App.toml" $ Text.unlines
          [ "[default]", "name = \"app\"", "port = 8080", "debug = true"
          , "[production]", "port = 80", "debug = false" ]
        dev <- F.loadExtractOrThrow (profiled F.Default)
        prod <- F.loadExtractOrThrow (profiled "production")
        dev @?= Config "app" 8080 True
        prod @?= Config "app" 80 False
    , testCase "§7 errors as values" $ do
        l <- ok (F.from (F.fromText F.toml "name = 1\n"))
        case F.extract l :: Either F.ConfigErrors Config of
          Left errs -> map F.errorKind (F.errorList errs)
            @?= [ F.InvalidType "integer 1" "a string", F.MissingField "port", F.MissingField "debug" ]
          Right v -> assertFailure (show v)
    , testCase "§8 decoding" $ do
        l <- ok (F.from (F.fromText F.yaml "listen_address: 0.0.0.0\nmax_connections: 100\nlog_level: info\n"))
        F.extract l @?= Right (Server "0.0.0.0" 100 Info)
        l2 <- ok (F.from (F.fromText F.json "{\"port\": 0, \"api\": {\"url\": \"https://x\"}}"))
        case F.extractAt (F.keyPath "port") l2 :: Either F.ConfigErrors Port of
          Left errs -> map F.errorKind (F.errorList errs) @?= [F.Message "a port must be between 1 and 65535"]
          Right v -> assertFailure (show v)
        F.extractAt (F.keyPath "api") l2 @?= Right (Endpoint "https://x" 30 3)
    , testCase "§9 provenance" $ Jail.withJail $ \j -> do
        path <- Jail.createFile j "conf/app.toml" "data_dir = \"data\"\ntoken = \"t0k3n\"\n"
        l <- ok (F.from (F.fileExact F.toml path))
        case F.extract l of
          Right (Paths d t) -> do
            F.relative d @?= Jail.jailDirectory j <> "/conf/data"
            fmap F.metaName (F.taggedMetadata t) @?= Just "TOML file"
          Left e -> assertFailure (Text.unpack (F.renderErrors e))
    , testCase "§10 partial" $ do
        l <- ok (F.from (F.fromText F.toml "appName = \"x\"\n"))
        F.extractPartial appDefaults l @?= (Right (App "x" 4 Nothing) :: Either F.ConfigErrors App)
        l2 <- ok (F.from (F.fromText F.toml ""))
        case F.extractPartial appDefaults l2 of
          Left errs -> map F.errorKind (F.errorList errs) @?= [F.MissingField "appName"]
          Right v -> assertFailure (show v)
    , testCase "§11 library figments compose" $ Jail.withJail $ \j -> do
        _ <- Jail.createFile j "MyApp.toml" "workers = 16\n"
        l <- ok applicationFigment
        F.extractAt (F.keyPath "workers") l @?= Right (16 :: Int)
    , testCase "§12 custom provider" $ do
        l <- ok (F.from (Secrets (Map.fromList [("token", "s3cret")])))
        F.extractAt (F.keyPath "token") l @?= Right ("s3cret" :: Text)
        fmap F.metaName (F.findMetadata (F.keyPath "token") l) @?= Just "secrets service"
    , testCase "§13 testing without global state" $ do
        l <- ok (F.from (Env.fromList [("APP_PORT", "1")] (F.prefixed "APP_")))
        F.extractAt (F.keyPath "port") l @?= Right (1 :: Int)
    ]

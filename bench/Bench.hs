-- | Benchmarks for the per-load path: parsing each format, combining
-- layers, and decoding (ADR 011).
module Main (main) where

import Control.DeepSeq (NFData, rnf)
import Criterion.Main
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Figment as F

data Service = Service {name :: Text, port :: Int, replicas :: Int, tags :: [Text]}
  deriving stock (Generic)
  deriving anyclass (F.FromConfig)

instance NFData Service where
  -- fields are strict enough for a benchmark; force them explicitly
  rnf (Service n p r ts) = rnf n `seq` rnf p `seq` rnf r `seq` rnf ts

-- | A config with @n@ services, in each format.
jsonDoc, tomlDoc, yamlDoc :: Int -> Text
jsonDoc n = "{" <> Text.intercalate "," [svc i | i <- [1 .. n]] <> "}"
  where
    svc i = "\"s" <> t i <> "\":{\"name\":\"svc" <> t i <> "\",\"port\":" <> t (8000 + i)
      <> ",\"replicas\":3,\"tags\":[\"a\",\"b\",\"c\"]}"
tomlDoc n = Text.concat [svc i | i <- [1 .. n]]
  where
    svc i = "[s" <> t i <> "]\nname = \"svc" <> t i <> "\"\nport = " <> t (8000 + i)
      <> "\nreplicas = 3\ntags = [\"a\", \"b\", \"c\"]\n"
yamlDoc n = Text.concat [svc i | i <- [1 .. n]]
  where
    svc i = "s" <> t i <> ":\n  name: svc" <> t i <> "\n  port: " <> t (8000 + i)
      <> "\n  replicas: 3\n  tags: [a, b, c]\n"

t :: Int -> Text
t = Text.pack . show

-- | Load and decode everything, forcing the result.
loadDecode :: F.Figment -> IO Int
loadDecode fig = do
  r <- F.loadExtract fig :: IO (Either F.ConfigErrors (Map.Map Text Service))
  case r of
    Left es -> fail (Text.unpack (F.renderErrors es))
    Right m -> pure $! (rnf m `seq` Map.size m)

main :: IO ()
main =
  defaultMain
    [ bgroup "load+decode 200 services"
        [ bench "json" $ nfIO (decodeFrom (F.fromText F.json (jsonDoc 200)))
        , bench "toml" $ nfIO (decodeFrom (F.fromText F.toml (tomlDoc 200)))
        , bench "yaml" $ nfIO (decodeFrom (F.fromText F.yaml (yamlDoc 200)))
        ]
    , bgroup "layers"
        [ bench "3 layers x 200 services" $ nfIO $ decodeLayers
            ( F.new
                & F.merge (F.fromText F.json (jsonDoc 200))
                & F.join (F.fromText F.toml (tomlDoc 200))
                & F.admerge (F.fromText F.yaml (yamlDoc 200))
            )
        ]
    ]
  where
    decodeFrom d = loadDecode (F.from d)
    decodeLayers = loadDecode

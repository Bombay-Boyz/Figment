{-# LANGUAGE OverloadedStrings #-}
-- | Criterion benchmarks for the two functions on the per-config-load
-- path (feasibility study §10, Standard §5.6).
module Figment.Bench (benchmarks) where

import Criterion.Main
import qualified Data.Text as Text

import Figment.Error (ConfigError)
import Figment.Layer (Layer(..), runLayers)
import Figment.Merge
import Figment.Value

smallRawJSON :: RawJSON
smallRawJSON = RawObj [("host", RawStr "localhost"), ("port", RawNum 8080)]

mediumRawJSON :: RawJSON
mediumRawJSON = RawObj
  [ (key i, RawStr "value") | i <- [0..99 :: Int] ]
  where key i = Text.pack ("k" ++ show i)

largeRawJSON :: RawJSON
largeRawJSON = RawObj
  [ (key i, RawObj
      [ ("nested", RawArr [RawNum (fromIntegral j) | j <- [0..9 :: Int]])
      , ("leaf",   RawStr "value")
      ])
  | i <- [0..999 :: Int]
  ]
  where key i = Text.pack ("k" ++ show i)

tag :: RawJSON -> Either ConfigError Tagged
tag = mkTagged defaultDepth (SourceId "bench", DefaultProfile)

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "mkTagged"
      [ bench "small"  $ nf tag smallRawJSON
      , bench "medium" $ nf tag mediumRawJSON
      , bench "large"  $ nf tag largeRawJSON
      ]
  , bgroup "mergeAlg"
      [ case (tag mediumRawJSON, tag mediumRawJSON) of
          (Right a, Right b) ->
            bench "100-key obj × 100-key obj" $
              nf (uncurry (mergeAlg RightWins Replace)) (a, b)
          _ -> bench "skipped" (nf id ())
      , case (tag largeRawJSON, tag largeRawJSON) of
          (Right a, Right b) ->
            bench "1000-key obj × 1000-key obj" $
              nf (uncurry (mergeAlg RightWins Replace)) (a, b)
          _ -> bench "skipped" (nf id ())
      ]
  , bgroup "runLayers"
      [ case (tag mediumRawJSON, tag mediumRawJSON, tag mediumRawJSON) of
          (Right a, Right b, Right c) ->
            bench "3-layer chain (medium)" $
              nf runLayers [Merge a, Join b, Admerge c]
          _ -> bench "skipped" (nf id ())
      ]
  ]

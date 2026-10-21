{-# LANGUAGE OverloadedStrings #-}
-- | Criterion benchmarks for the two functions on the per-config-load
-- path (feasibility study §10, Standard §5.6). Phase 1 benchmarks only;
-- regression thresholds get wired into CI once we have a baseline from
-- a first successful run.
module Figment.Bench (benchmarks) where

import Criterion.Main
import Data.Text (Text)
import qualified Data.Text as Text

import Figment.Value
import Figment.Merge

-- | A realistically large synthetic config: ~1000 top-level keys, each
-- with a 10-element array and a leaf — the "large-input case" of
-- Standard §4.10.
largeRawJSON :: RawJSON
largeRawJSON = RawObj
  [ (key i, RawObj
      [ ("nested", RawArr [RawNum (fromIntegral j) | j <- [0..9 :: Int]])
      , ("leaf",   RawStr "value")
      ])
  | i <- [0..999]
  ]
  where
    key :: Int -> Text
    key i = Text.pack ("k" ++ show i)

benchmarks :: [Benchmark]
benchmarks =
  case (mkTagged defaultDepth (SourceId "bench", DefaultProfile) largeRawJSON,
        mkTagged defaultDepth (SourceId "bench", DefaultProfile) largeRawJSON) of
    (Right a, Right b) -> [ bgroup "mkTagged"
                              [ bench "large (1000 keys × 10-item arrays)" $
                                  nf (mkTagged defaultDepth (SourceId "bench", DefaultProfile))
                                     largeRawJSON
                              ]
                          , bgroup "mergeAlg"
                              [ bench "1000-key obj × 1000-key obj" $
                                  nf (uncurry (mergeAlg RightWins Replace)) (a, b)
                              ]
                          ]
    _ -> [ bgroup "mkTagged" []
         , bgroup "mergeAlg" []
         ]

{-# LANGUAGE Safe #-}
-- | Mechanical proof of ADR 014: the pure core is importable from a
-- 'Safe' module. If this module compiles, GHC has verified that no
-- module in the import closure (Prim, Value, Merge, Layer, Error) uses
-- unsafePerformIO or violates type safety / referential transparency.
--
-- This module cannot use tasty because tasty is not Safe. It exports
-- plain values that the test harness (which is Trustworthy by default)
-- checks.
module Figment.SafeBoundary (safeBoundaryProbe) where

import qualified Data.Map.Strict as Map

import Figment.Value (Tagged(..), Value(..), Metadata(..))
import Figment.Merge (Bias(..), ArrayPolicy(..), mergeAlg)
import Figment.Layer (Layer(..), runLayers)

-- | Exercise every Safe-marked module. Correctness is checked by the
-- caller (test/Figment/SafeBoundaryTest.hs); the point of this module
-- is that it typechecks under Safe.
safeBoundaryProbe :: Bool
safeBoundaryProbe =
  let empty = Tagged (VObj Map.empty) Merged
      merged = mergeAlg RightWins Replace empty empty
      layered = runLayers ([] :: [Layer])
  in merged == layered && merged == empty

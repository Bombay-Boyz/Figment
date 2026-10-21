{-# LANGUAGE Trustworthy #-}
-- | Provenance-preserving extraction (ADR 012). Collects every leaf's
-- 'Metadata' into a 'ProvMap' keyed by its 'Path', alongside the
-- decoded record from 'runExtract'.
module Figment.Prov
  ( ProvMap
  , runExtractProv
  , provOf
  ) where

import Data.Aeson (FromJSON)
import qualified Data.Map.Strict as Map

import Figment.Error (ConfigError)
import Figment.Extract (runExtract)
import Figment.Value
  ( Tagged(..), Value(..), Metadata(..), Key(..), Path(..), extend, extendIx )

-- | A leaf path → its metadata. Every node that carries a @'Leaf'@ tag
-- in the source tree appears here; @'Merged'@ containers do not.
type ProvMap = Map.Map Path Metadata

-- | Decode a merged tree into @a@ (via 'runExtract') and additionally
-- return the per-leaf provenance of the source tree.
runExtractProv
  :: FromJSON a
  => Tagged -> Either ConfigError (a, ProvMap)
runExtractProv t = do
  a <- runExtract t
  pure (a, collectProv t)

-- | Walk the tree, accumulating @path ↦ Metadata@ for every leaf.
-- Total on every 'Value'; containers are recursed into.
collectProv :: Tagged -> ProvMap
collectProv (Tagged v m) = go (Path []) v m
  where
    go :: Path -> Value -> Metadata -> ProvMap
    go p VNull          meta = leaf p meta
    go p (VBool _)      meta = leaf p meta
    go p (VNum _)       meta = leaf p meta
    go p (VStr _)       meta = leaf p meta
    go p (VArr xs)      _    = Map.unions
      [ go (extendIx p i) (tVal x) (tMeta x) | (i, x) <- zip [0..] xs ]
    go p (VObj obj)     _    = Map.unions
      [ go (extend p (unKey k)) (tVal sub) (tMeta sub)
      | (k, sub) <- Map.toList obj ]

    leaf :: Path -> Metadata -> ProvMap
    leaf p meta = Map.singleton p meta

    unKey (Key t) = t

-- | Look up a leaf's provenance by its path. Total; returns 'Nothing'
-- if the path is not a leaf in the map.
provOf :: Path -> ProvMap -> Maybe Metadata
provOf = Map.lookup

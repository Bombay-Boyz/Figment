-- | Combining values: the four strategies of Rust figment, with the
-- same semantics (see @coalesce.rs@ upstream).
--
-- * 'Merge'   — on conflict the incoming value wins
-- * 'Join'    — on conflict the existing value wins
-- * 'Adjoin'  — like 'Join', but arrays are concatenated
-- * 'Admerge' — like 'Merge', but arrays are concatenated
--
-- Dictionaries are always merged key by key, recursively. A @null@ is a
-- value like any other: merging @null@ over a key replaces it.
--
-- Laws (property-tested in @test/Figment/MergeLaws.hs@):
--
-- * @coalesce Join a b == coalesce Merge b a@ (duality, tags included)
-- * @coalesce o v v@ has the same data as @v@ for 'Merge' and 'Join'
-- * the empty dictionary is a two-sided identity on dictionaries
-- * for a fixed order, 'coalesce' is associative on trees of identical
--   shape. It is /not/ associative in general (a scalar between two
--   dictionaries), which is why t'Value' has no 'Semigroup' instance; the
--   lawful monoid lives one level up, on 'Figment.Figment'.
module Figment.Merge
  ( Order (..)
  , coalesce
  , coalesceDict
  , coalesceData
  , coalesceProfile
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Figment.Profile
import Figment.Value

-- | How a provider's values combine with the values already present.
data Order = Merge | Join | Adjoin | Admerge
  deriving stock (Eq, Ord, Show, Enum, Bounded)

leftWins :: Order -> Bool
leftWins Join = True
leftWins Adjoin = True
leftWins _ = False

concatenates :: Order -> Bool
concatenates Adjoin = True
concatenates Admerge = True
concatenates _ = False

-- | Combine an existing value (left) with an incoming one (right).
coalesce :: Order -> Value -> Value -> Value
coalesce o a@(Value ta na) b@(Value tb nb) = case (na, nb) of
  (Dict da, Dict db) ->
    Value (if leftWins o then ta else tb) (Dict (coalesceDict o da db))
  (Array xa, Array xb)
    | concatenates o -> Value ta (Array (xa <> xb))
  _ -> if leftWins o then a else b

-- | Combine two dictionaries key by key.
coalesceDict :: Order -> Dict -> Dict -> Dict
coalesceDict o = Map.unionWith (coalesce o)

-- | Combine per-profile data.
coalesceData :: Order -> Map Profile Dict -> Map Profile Dict -> Map Profile Dict
coalesceData o = Map.unionWith (coalesceDict o)

-- | Combine selected-profile hints.
coalesceProfile :: Order -> Profile -> Profile -> Profile
coalesceProfile o a b = if leftWins o then a else b

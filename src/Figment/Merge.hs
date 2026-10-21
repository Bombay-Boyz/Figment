-- | The merge operator '⊕' (§2, §5 of the feasibility study).
--
-- There is deliberately /no/ 'Semigroup' or 'Monoid' instance on
-- 'Tagged' (Standard §0.3, feasibility study §7): associativity holds
-- only under schema-uniformity, so offering the instance would be a
-- lawless instance a caller could legally reassociate. Same-strategy
-- associativity is instead tested as a property (§10).
module Figment.Merge
  ( Bias(..)
  , ArrayPolicy(..)
  , mergeAlg
  , pick
  ) where

import qualified Data.Map.Strict as Map

import Figment.Value (Tagged(..), Value(..), Metadata(..))

data Bias        = LeftWins | RightWins  deriving (Eq, Show)
data ArrayPolicy = Replace  | Concat     deriving (Eq, Show)

-- | The merge algebra. Three-way pattern match on (v1, v2, ap).
-- 'ArrayPolicy' is a closed sum, so GHC checks exhaustiveness and no
-- 'Eq ArrayPolicy' constraint is needed (Standard §0.10).
mergeAlg :: Bias -> ArrayPolicy -> Tagged -> Tagged -> Tagged
mergeAlg bias ap t1@(Tagged v1 _) t2@(Tagged v2 _) = case (v1, v2, ap) of
  (VObj o1, VObj o2, _) ->
    Tagged (VObj (Map.unionWith (mergeAlg bias ap) o1 o2)) Merged
  (VArr a1, VArr a2, Concat) ->
    Tagged (VArr (a1 ++ a2)) Merged
  (VArr _, VArr _, Replace) ->
    pick bias t1 t2
  (_, _, _) ->
    pick bias t1 t2

pick :: Bias -> a -> a -> a
pick LeftWins  x _ = x
pick RightWins _ y = y

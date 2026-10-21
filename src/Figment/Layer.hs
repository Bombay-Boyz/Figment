{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}
-- | Cross-strategy composition (§7 of the feasibility study).
--
-- The one place strategies are interpreted: a strict left fold matching
-- the Corollary of §3. There is no 'Semigroup' on 'Layer' — mixed
-- chains cannot be silently reassociated, by construction
-- (Standard §2.8: a mechanical, not a documented, guarantee).
module Figment.Layer
  ( Layer(..)
  , emptyObj
  , runLayers
  ) where

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif
import qualified Data.Map.Strict as Map

import Figment.Merge (Bias(..), ArrayPolicy(..), mergeAlg)
import Figment.Value (Tagged(..), Value(..), Metadata(..))

data Layer
  = Join    Tagged   -- ^ LeftWins,  Replace
  | Merge   Tagged   -- ^ RightWins, Replace
  | Adjoin  Tagged   -- ^ LeftWins,  Concat
  | Admerge Tagged   -- ^ RightWins, Concat

emptyObj :: Tagged
emptyObj = Tagged (VObj Map.empty) Merged

-- | The one place strategies are interpreted: a strict left fold.
runLayers :: [Layer] -> Tagged
runLayers = foldl' step emptyObj
  where
    step acc (Join    t) = mergeAlg LeftWins  Replace acc t
    step acc (Merge   t) = mergeAlg RightWins Replace acc t
    step acc (Adjoin  t) = mergeAlg LeftWins  Concat  acc t
    step acc (Admerge t) = mergeAlg RightWins Concat  acc t

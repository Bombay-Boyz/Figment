-- | Resource limits applied while reading untrusted sources.
module Figment.Limits
  ( Limits
  , mkLimits
  , defaultLimits
  , maxDepth
  , maxNodes
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Figment.Error

-- | Bounds enforced /inside/ every parser figment ships (not after the
-- fact), so a hostile source is rejected before it can exhaust memory.
-- The constructor is hidden: obtain values via 'mkLimits'.
data Limits = Limits
  { maxDepth :: !Int
    -- ^ maximum container nesting (the root dictionary is depth 1)
  , maxNodes :: !Int
    -- ^ maximum number of values per source, counting YAML alias
    -- expansions (defeats \"billion laughs\")
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Both limits must be positive.
mkLimits :: Int -> Int -> Either ConfigError Limits
mkLimits d n
  | d <= 0 = Left (bad "maxDepth" d)
  | n <= 0 = Left (bad "maxNodes" n)
  | otherwise = Right (Limits d n)
  where
    bad name v = configError (InvalidArgument (name <> " must be positive, got " <> Text.pack (show v)))

-- | Depth 64, one million values (ADR 001, ADR 017).
defaultLimits :: Limits
defaultLimits = Limits 64 1000000

-- | Resource limits applied while reading untrusted sources.
module Figment.Limits
  ( Limits
  , mkLimits
  , withMaxBytes
  , defaultLimits
  , defaultMaxBytes
  , maxDepth
  , maxNodes
  , maxBytes
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Figment.Error

-- | Bounds enforced while a source is read, so that a hostile or
-- accidentally enormous source is rejected before it can exhaust memory.
-- The constructor is hidden: obtain values via 'mkLimits' and
-- 'withMaxBytes'.
--
-- * the size of a source is checked /before/ it is parsed (files are read
--   incrementally and abandoned as soon as they exceed 'maxBytes');
-- * nesting depth and the number of values are checked /while/ the tree is
--   built (JSON, environment and command-line values), or on the parser's
--   intermediate form (TOML, YAML), which is itself bounded by 'maxBytes'.
data Limits = Limits
  { maxDepth :: !Int
    -- ^ maximum container nesting (the root dictionary is depth 1)
  , maxNodes :: !Int
    -- ^ maximum number of values per source, counting YAML alias
    -- expansions (defeats \"billion laughs\")
  , maxBytes :: !Int
    -- ^ maximum size of one source in bytes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Depth and node limits, with the default size limit
-- ('defaultMaxBytes'). Both must be positive; adjust the size with
-- 'withMaxBytes'.
mkLimits :: Int -> Int -> Either ConfigError Limits
mkLimits d n
  | d <= 0 = Left (invalid "maxDepth" d)
  | n <= 0 = Left (invalid "maxNodes" n)
  | otherwise = Right (Limits d n defaultMaxBytes)

-- | Replace the size limit. It must be positive.
withMaxBytes :: Int -> Limits -> Either ConfigError Limits
withMaxBytes b l
  | b <= 0 = Left (invalid "maxBytes" b)
  | otherwise = Right l {maxBytes = b}

invalid :: Text.Text -> Int -> ConfigError
invalid name v =
  configError (InvalidArgument (name <> " must be positive, got " <> Text.pack (show v)))

-- | Depth 64, 100 000 values, 8 MiB per source (ADR 001, ADR 017). A
-- hand-written configuration has hundreds of values; these limits leave
-- two orders of magnitude of headroom. They are low enough that the
-- worst document the TOML parser can be handed (it builds the whole tree
-- before figment sees a value, roughly a kilobyte per value) stays well
-- under a gigabyte. Raise them with 'mkLimits' and 'withMaxBytes' if you
-- load data files rather than configuration.
defaultLimits :: Limits
defaultLimits = Limits 64 100000 defaultMaxBytes

-- | 8 MiB: far above any hand-written configuration file.
defaultMaxBytes :: Int
defaultMaxBytes = 8 * 1024 * 1024

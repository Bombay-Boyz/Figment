module Figment.Value
  ( Key(..)
  , SourceId(..)
  , Path(..)
  , Profile(..)
  , ProfileName(..)
  , Value(..)
  , Metadata(..)
  , Tagged(..)
  , leafInfo
  , Depth
  , mkDepth
  , defaultDepth
  , RawJSON(..)
  , mkTagged
  , extend
  , extendIx
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import Data.Text (Text)

import Figment.Error (ConfigError(..))
import Figment.Prim
  ( Key(..), SourceId(..), Path(..), Profile(..), ProfileName(..)
  , Depth(..), defaultDepth, extend, extendIx
  )

mkDepth :: Int -> Either ConfigError Depth
mkDepth n
  | n > 0     = Right (UnsafeDepth n)
  | otherwise = Left (ConfigurationError "Depth" "must be a positive Int")

data RawJSON
  = RawNull
  | RawBool Bool
  | RawNum  Scientific
  | RawStr  Text
  | RawObj  [(Text, RawJSON)]
  | RawArr  [RawJSON]
  deriving (Show)

data Value
  = VNull
  | VBool   Bool
  | VNum    Scientific
  | VStr    Text
  | VArr    [Tagged]
  | VObj    (Map Key Tagged)
  deriving (Eq, Show, Generic)
instance NFData Value where
  rnf VNull        = ()
  rnf (VBool b)    = rnf b
  rnf (VNum n)     = rnf n
  rnf (VStr t)     = rnf t
  rnf (VArr xs)    = rnf xs
  rnf (VObj m)     = rnf m

-- | 'Metadata' mirrors '𝕄 = Maybe (SourceId, Profile, Path)' exactly
-- ('Merged' ↔ 'Nothing', 'Leaf' ↔ 'Just').
--
-- 'Leaf' is a positional constructor — no record syntax — because record
-- selectors on a multi-constructor type are partial (they panic on
-- 'Merged'), which Standard §5.2 bans outright. The total accessor
-- 'leafInfo' below is the only way to reach the components.
data Metadata
  = Merged
  | Leaf SourceId Profile Path
  deriving (Eq, Show, Generic)
instance NFData Metadata where
  rnf Merged          = ()
  rnf (Leaf s p pa)   = rnf s `seq` rnf p `seq` rnf pa

-- | The total accessor for 'Leaf': @'Just' (s, p, path)@ for a leaf,
-- 'Nothing' for 'Merged'. Replaces what would otherwise be three
-- partial record selectors.
leafInfo :: Metadata -> Maybe (SourceId, Profile, Path)
leafInfo Merged                    = Nothing
leafInfo (Leaf src prof path')     = Just (src, prof, path')

data Tagged = Tagged { tVal :: Value, tMeta :: Metadata }
  deriving (Eq, Show, Generic)
instance NFData Tagged where rnf (Tagged v m) = rnf v `seq` rnf m

mkTagged :: Depth -> (SourceId, Profile) -> RawJSON -> Either ConfigError Tagged
mkTagged total@(UnsafeDepth budget) (s, p) = go budget (Path [])
  where
    go :: Int -> Path -> RawJSON -> Either ConfigError Tagged
    go remaining curPath raw
      | remaining <= 0 = Left (DepthExceeded curPath total)
      | otherwise = case raw of
          RawObj fields ->
            (VObj . Map.fromList <$>
              traverse (\(k, v) ->
                          (,) (Key k) <$> go (remaining - 1) (extend curPath k) v)
                       fields)
              `taggedAs` curPath
          RawArr xs ->
            (VArr <$>
              traverse (\(i, v) -> go (remaining - 1) (extendIx curPath i) v)
                       (zip [0 :: Int ..] xs))
              `taggedAs` curPath
          RawNull   -> pure VNull      `taggedAs` curPath
          RawBool b -> pure (VBool b)  `taggedAs` curPath
          RawNum  n -> pure (VNum n)   `taggedAs` curPath
          RawStr  t -> pure (VStr t)   `taggedAs` curPath
      where
        taggedAs mv p' = (\v -> Tagged v (Leaf s p p')) <$> mv

-- | Interoperability with aeson: reuse existing 'Data.Aeson.FromJSON' / 'Data.Aeson.ToJSON'
-- instances, and serialise Haskell values as a provider.
module Figment.Aeson
  ( -- * Decoding via FromJSON
    ViaAeson (..)
  , aesonDecoder
    -- * Conversion
  , toAeson
  , fromAeson
    -- * The Serialized provider
  , Serialized
  , defaults
  , globals
  , keyed
  , serializedIn
  , serializedNamed
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AT
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GHC.Stack (CallStack, HasCallStack, SrcLoc, callStack, getCallStack)

import Figment.Core
import Figment.Decode
import Figment.Error
import Figment.Internal.Budget
import Figment.Limits
import Figment.Metadata
import Figment.Path
import Figment.Profile
import Figment.Value

-- | Decode with an existing 'Data.Aeson.FromJSON' instance:
--
-- > newtype Port = Port Int deriving newtype FromJSON
-- >   deriving FromConfig via ViaAeson Port
--
-- aeson's error path is translated into a figment t'Path', so the error
-- still points at the right key, file, line and column.
newtype ViaAeson a = ViaAeson {unViaAeson :: a}

instance Aeson.FromJSON a => FromConfig (ViaAeson a) where
  fromConfig = ViaAeson <$> aesonDecoder
  missingValue = ViaAeson <$> Aeson.omittedField

-- | A decoder from a 'Aeson.FromJSON' instance.
aesonDecoder :: Aeson.FromJSON a => Decoder a
aesonDecoder = mkDecoder $ \c v -> case AT.iparse Aeson.parseJSON (toAeson v) of
  AT.ISuccess a -> Success a
  AT.IError jpath msg ->
    let rel = fromSegments (map seg jpath)
        target = fromMaybe v (lookupPath rel v)
        t = valueTag target
    in Failure $ single
         ConfigError
           { errorKind = Message (Text.pack msg)
           , errorPath = ctxPath c <> rel
           , errorProfile = Just (fromMaybe (ctxProfile c) (tagProfile t))
           , errorMetadata = tagMeta t
           , errorPos = tagPos t
           , errorHints = []
           }
  where
    seg (AT.Key k) = Figment.Path.Key (Key.toText k)
    seg (AT.Index i) = Figment.Path.Index i

-- | Forget provenance and produce an aeson value. Non-finite numbers
-- (which JSON cannot represent) become strings.
toAeson :: Value -> Aeson.Value
toAeson (Value _ n) = case n of
  Null -> Aeson.Null
  Bool b -> Aeson.Bool b
  Num (Finite s) -> Aeson.Number s
  Num PosInfinity -> Aeson.String "inf"
  Num NegInfinity -> Aeson.String "-inf"
  Num NaN -> Aeson.String "nan"
  String t -> Aeson.String t
  Array xs -> Aeson.Array (fmap toAeson xs)
  Dict d -> Aeson.Object (KeyMap.fromList [(Key.fromText k, toAeson x) | (k, x) <- Map.toList d])

-- | Convert an aeson value, enforcing limits.
fromAeson :: Limits -> Aeson.Value -> Either ConfigError Value
fromAeson lim = runBudget lim . go 1 root
  where
    go :: Int -> Path -> Aeson.Value -> Budget Value
    go d p v = do
      spend 1 p Nothing
      case v of
        Aeson.Null -> pure (mkValue Null)
        Aeson.Bool b -> pure (mkValue (Bool b))
        Aeson.Number s -> pure (mkValue (Num (Finite s)))
        Aeson.String t -> pure (mkValue (String t))
        Aeson.Array xs -> do
          enterContainer d p Nothing
          ys <- traverse (\(i, x) -> go (d + 1) (p /# i) x) (Vector.indexed xs)
          pure (mkValue (Array ys))
        Aeson.Object o -> do
          enterContainer d p Nothing
          kvs <- traverse (\(k, x) -> let kt = Key.toText k in (,) kt <$> go (d + 1) (p /. kt) x) (KeyMap.toList o)
          pure (mkValue (Dict (Map.fromList kvs)))

-- | A provider serialising a Haskell value with its 'Aeson.ToJSON'
-- instance (Rust: @Serialized@).
data Serialized = Serialized
  { serValue :: Aeson.Value
  , serProfile :: Profile
  , serKey :: Maybe Path
  , serName :: Text
  , serLoc :: Maybe SrcLoc
  }

-- | Values for the 'Default' profile.
defaults :: (HasCallStack, Aeson.ToJSON a) => a -> Serialized
defaults a = Serialized (Aeson.toJSON a) Default Nothing "serialized defaults" (here callStack)

-- | Values for the 'Global' profile (override every profile).
globals :: (HasCallStack, Aeson.ToJSON a) => a -> Serialized
globals a = Serialized (Aeson.toJSON a) Global Nothing "serialized globals" (here callStack)

-- | A single value at a dotted key path, e.g. @keyed "db.port" 5432@.
keyed :: (HasCallStack, Aeson.ToJSON a) => Text -> a -> Serialized
keyed k a = Serialized (Aeson.toJSON a) Default (Just (keyPath k)) ("serialized value for `" <> k <> "`") (here callStack)

-- | Put the values in another profile.
serializedIn :: Profile -> Serialized -> Serialized
serializedIn p s = s {serProfile = p}

-- | Rename the provider (shown in error messages).
serializedNamed :: Text -> Serialized -> Serialized
serializedNamed n s = s {serName = n}

here :: CallStack -> Maybe SrcLoc
here cs = case getCallStack cs of
  ((_, l) : _) -> Just l
  [] -> Nothing

instance Provider Serialized where
  metadata s = (named (serName s)) {metaSource = CodeSource <$> serLoc s}
  provide lim s = pure $ case fromAeson lim (serValue s) of
    Left e -> Left (single e)
    Right v -> case serKey s of
      Just p -> Right (Map.singleton (serProfile s) (wrap (segments p) v))
      Nothing -> case valueNode v of
        Dict d -> Right (Map.singleton (serProfile s) d)
        n -> Left (single (configError (InvalidType (describe n) "a dictionary (serialize a record or a map, or use `keyed`)")))
    where
      wrap [] v = case valueNode v of
        Dict d -> d
        _ -> Map.empty
      wrap [Figment.Path.Key k] v = Map.singleton k v
      wrap (Figment.Path.Key k : rest) v = Map.singleton k (mkValue (Dict (wrap rest v)))
      wrap (Figment.Path.Index _ : rest) v = wrap rest v

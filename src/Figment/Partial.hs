{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Higher-kinded partial configurations.
--
-- Write a configuration record once, parameterised by a functor:
--
-- > data AppF f = App
-- >   { host :: Field f Text
-- >   , port :: Field f Int
-- >   , db   :: DbF f            -- nested partial record
-- >   } deriving stock Generic
-- > instance Partial AppF
-- > type App = AppF Identity      -- fields are plain: host :: Text
--
-- @AppF Maybe@ is the /partial/ form: every field optional. Defaults
-- are written as a partial value, merged under whatever the providers
-- supplied, and 'complete' turns the result into the total @AppF
-- Identity@, reporting /every/ missing field (with the env var, CLI
-- flag, or file key that would have set it) rather than the first.
--
-- The compile-time guarantee: 'Figment.extractTotal' takes a total
-- @AppF Identity@ as its defaults, so a missing-field error is
-- impossible by construction; only type errors remain.
module Figment.Partial
  ( Field
  , Partial (..)
  , GPartial
  ) where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Figment.Decode
import Figment.Error
import Figment.Path
import Figment.Value

-- | @Field Identity a = a@, so the total record has ordinary fields;
-- @Field Maybe a = Maybe a@ for the partial record.
type family Field (f :: Type -> Type) (a :: Type) :: Type where
  Field Identity a = a
  Field f a = f a

-- | Records whose fields are 'Field's or nested 'Partial' records.
-- Every method has a generic default: @instance Partial AppF@ suffices.
class Partial (c :: (Type -> Type) -> Type) where
  -- | Decode whatever is present; absent fields are 'Nothing'. Type
  -- errors in present fields are reported (all of them).
  partialDecoder :: Options -> Decoder (c Maybe)
  default partialDecoder
    :: (Generic (c Maybe), GPartial (Rep (c Maybe)) (Rep (c Identity)))
    => Options -> Decoder (c Maybe)
  partialDecoder o = to <$> mkDecoder (\c v -> case valueNode v of
    Dict m -> recordCheck o (gpnames @(Rep (c Maybe)) @(Rep (c Identity)) o) c m
                (gpfields @(Rep (c Maybe)) @(Rep (c Identity)) o c m)
    n -> Failure (single (atValue c v (InvalidType (describe n) "a dictionary"))))

  -- | Right-biased combination: fields set on the right win.
  overlay :: c Maybe -> c Maybe -> c Maybe
  default overlay
    :: (Generic (c Maybe), GPartial (Rep (c Maybe)) (Rep (c Identity)))
    => c Maybe -> c Maybe -> c Maybe
  overlay a b = to (goverlay @(Rep (c Maybe)) @(Rep (c Identity)) (from a) (from b))

  -- | The partial value with nothing set.
  emptyPartial :: c Maybe
  default emptyPartial
    :: (Generic (c Maybe), GPartial (Rep (c Maybe)) (Rep (c Identity)))
    => c Maybe
  emptyPartial = to (gempty @(Rep (c Maybe)) @(Rep (c Identity)))

  -- | The total value, or the (relative) paths of every required field
  -- that is still missing. Fields whose type has a 'missingValue'
  -- (e.g. 'Maybe') are never missing.
  complete :: Options -> c Maybe -> Either (NonEmpty Path) (c Identity)
  default complete
    :: (Generic (c Maybe), Generic (c Identity), GPartial (Rep (c Maybe)) (Rep (c Identity)))
    => Options -> c Maybe -> Either (NonEmpty Path) (c Identity)
  complete o p = case gcomplete @(Rep (c Maybe)) @(Rep (c Identity)) o root (from p) of
    Left (x : xs) -> Left (x :| xs)
    Left [] -> Left (root :| [])
    Right r -> Right (to r)

  -- | Embed a total value (every field set).
  total :: c Identity -> c Maybe
  default total
    :: (Generic (c Maybe), Generic (c Identity), GPartial (Rep (c Maybe)) (Rep (c Identity)))
    => c Identity -> c Maybe
  total = to . gtotal @(Rep (c Maybe)) @(Rep (c Identity)) . from

-- | Accumulating completion.
type Completion a = Either [Path] a

both :: Completion (a -> b) -> Completion a -> Completion b
both (Left a) (Left b) = Left (a ++ b)
both (Left a) (Right _) = Left a
both (Right _) (Left b) = Left b
both (Right f) (Right x) = Right (f x)

-- | Generic machinery behind 'Partial's defaults.
class GPartial (pm :: Type -> Type) (pi :: Type -> Type) where
  gpfields :: Options -> DecodeContext -> Dict -> Result (pm x)
  gpnames :: Options -> [Text]
  goverlay :: pm x -> pm x -> pm x
  gempty :: pm x
  gcomplete :: Options -> Path -> pm x -> Completion (pi x)
  gtotal :: pi x -> pm x

instance GPartial pm pi => GPartial (D1 d pm) (D1 d pi) where
  gpfields o c m = M1 <$> gpfields @pm @pi o c m
  gpnames o = gpnames @pm @pi o
  goverlay (M1 a) (M1 b) = M1 (goverlay @pm @pi a b)
  gempty = M1 (gempty @pm @pi)
  gcomplete o p (M1 a) = M1 <$> gcomplete @pm @pi o p a
  gtotal (M1 a) = M1 (gtotal @pm @pi a)

instance GPartial pm pi => GPartial (C1 c pm) (C1 c pi) where
  gpfields o c m = M1 <$> gpfields @pm @pi o c m
  gpnames o = gpnames @pm @pi o
  goverlay (M1 a) (M1 b) = M1 (goverlay @pm @pi a b)
  gempty = M1 (gempty @pm @pi)
  gcomplete o p (M1 a) = M1 <$> gcomplete @pm @pi o p a
  gtotal (M1 a) = M1 (gtotal @pm @pi a)

instance (GPartial am ai, GPartial bm bi) => GPartial (am :*: bm) (ai :*: bi) where
  gpfields o c m = (:*:) <$> gpfields @am @ai o c m <*> gpfields @bm @bi o c m
  gpnames o = gpnames @am @ai o ++ gpnames @bm @bi o
  goverlay (a :*: b) (a' :*: b') = goverlay @am @ai a a' :*: goverlay @bm @bi b b'
  gempty = gempty @am @ai :*: gempty @bm @bi
  gcomplete o p (a :*: b) = fmap (:*:) (gcomplete @am @ai o p a) `both` gcomplete @bm @bi o p b
  gtotal (a :*: b) = gtotal @am @ai a :*: gtotal @bm @bi b

instance GPartial U1 U1 where
  gpfields _ _ _ = Success U1
  gpnames _ = []
  goverlay _ _ = U1
  gempty = U1
  gcomplete _ _ _ = Right U1
  gtotal _ = U1

-- | A leaf field: @Field f a@.
instance (Selector s, FromConfig a) => GPartial (S1 s (Rec0 (Maybe a))) (S1 s (Rec0 a)) where
  gpfields o c m =
    let name = selKey @s o
    in case Map.lookup name m of
         Just v -> M1 . K1 . Just <$> runIn c {ctxPath = ctxPath c /. name} v
         Nothing -> Success (M1 (K1 Nothing))
  gpnames o = [selKey @s o]
  goverlay (M1 (K1 a)) (M1 (K1 b)) = M1 (K1 (b <|> a))
  gempty = M1 (K1 Nothing)
  gcomplete o p (M1 (K1 m)) = case m <|> missingValue @a of
    Just a -> Right (M1 (K1 a))
    Nothing -> Left [p /. selKey @s o]
  gtotal (M1 (K1 a)) = M1 (K1 (Just a))

-- | A nested partial record.
instance (Selector s, Partial b) => GPartial (S1 s (Rec0 (b Maybe))) (S1 s (Rec0 (b Identity))) where
  gpfields o c m =
    let name = Text.pack (fieldLabelModifier o (selName (SP :: SP s U1 ())))
    in case Map.lookup name m of
         Just v -> M1 . K1 <$> runDecoderIn (partialDecoder @b o) c {ctxPath = ctxPath c /. name} v
         Nothing -> Success (M1 (K1 (emptyPartial @b)))
  gpnames o = [Text.pack (fieldLabelModifier o (selName (SP :: SP s U1 ())))]
  goverlay (M1 (K1 a)) (M1 (K1 b)) = M1 (K1 (overlay @b a b))
  gempty = M1 (K1 (emptyPartial @b))
  gcomplete o p (M1 (K1 m)) =
    let here = p /. Text.pack (fieldLabelModifier o (selName (SP :: SP s U1 ())))
    in case complete @b o m of
         Right r -> Right (M1 (K1 r))
         Left ps -> Left (map (here <>) (NE.toList ps))
  gtotal (M1 (K1 a)) = M1 (K1 (total @b a))

instance
  TypeError ('Text "figment: a Partial configuration must be a record with a single constructor")
  => GPartial (f :+: g) (f' :+: g')
  where
  gpfields _ _ _ = Failure (single (configError (Message "unreachable")))
  gpnames _ = []
  goverlay a _ = a
  gempty = gempty @(f :+: g) @(f' :+: g')
  gcomplete _ _ _ = Left []
  gtotal _ = gempty @(f :+: g) @(f' :+: g')

-- helpers ----------------------------------------------------------------

data SP (s :: Meta) (f :: Type -> Type) a = SP

selKey :: forall (s :: Meta). Selector s => Options -> Text
selKey o = Text.pack (fieldLabelModifier o (selName (SP :: SP s U1 ())))

-- | Run 'fromConfig' at an explicit context.
runIn :: FromConfig a => DecodeContext -> Value -> Result a
runIn = decodeWith fromConfig

runDecoderIn :: Decoder a -> DecodeContext -> Value -> Result a
runDecoderIn = decodeWith

-- | Unknown-field check, accumulated together with the field results.
recordCheck :: Options -> [Text] -> DecodeContext -> Dict -> Result a -> Result a
recordCheck o known c m fields =
  let knownSet = Set.fromList known
      unknown = [kv | rejectUnknownFields o, kv@(key, _) <- Map.toList m, not (key `Set.member` knownSet)]
      errs =
        [ single (atValue c {ctxPath = ctxPath c /. key} x (UnknownField key (suggestAt i key known)))
        | (i, (key, x)) <- zip [0 ..] unknown
        ]
   in case NE.nonEmpty errs of
        Nothing -> fields
        Just es -> Failure (sconcat es) <* fields

atValue :: DecodeContext -> Value -> ErrorKind -> ConfigError
atValue c x kind =
  let t = valueTag x
  in ConfigError kind (ctxPath c) (tagProfile t) (tagMeta t) (tagPos t) []

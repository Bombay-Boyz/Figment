{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Decoding configuration values into Haskell types.
--
-- t'Decoder' is an /accumulating/ applicative: independent failures (two
-- bad fields, a missing field and an out-of-range number, ...) are all
-- reported, each with its exact path, source, line and column. There is
-- deliberately no 'Monad' instance, because a monad must short-circuit;
-- use 'andThen' when a step genuinely depends on an earlier result.
module Figment.Decode
  ( -- * Decoders
    Decoder
  , Result (..)
  , DecodeContext (..)
  , rootContext
  , runDecoder
  , decodeWith
  , mkDecoder
  , resultToEither
    -- * The class
  , FromConfig (..)
  , FromConfigKey (..)
    -- * Combinators
  , andThen
  , withValue
  , currentPath
  , failWith
  , validate
  , field
  , fieldWith
  , optionalField
  , fieldOr
  , arrayOf
  , dictOf
  , textual
  , boundedIntegral
    -- * Generic deriving
  , Options (..)
  , defaultOptions
  , snakeCase
  , kebabCase
  , genericDecoder
  , GDecode
    -- * Magic types
  , Tagged (..)
  , taggedMetadata
  , RelativePath (..)
  , relative
    -- * Internals shared with "Figment.Partial"
  , missingFieldError
  , suggest
  , suggestAt
  ) where

import Data.Char (isDigit, isLower, isUpper, toLower)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Semigroup (sconcat)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (ISO8601, iso8601ParseM)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Numeric.Natural (Natural)
import System.FilePath (isAbsolute, takeDirectory, (</>))

import Figment.Error
import Figment.Internal.ValueParser (parseNumber)
import Figment.Metadata
import Figment.Path
import Figment.Profile
import Figment.Value

------------------------------------------------------------------------
-- Results and decoders

-- | Success, or every failure found.
data Result a
  = Failure !ConfigErrors
  | Success a
  deriving stock (Show, Functor)

instance Applicative Result where
  pure = Success
  Failure a <*> Failure b = Failure (a <> b)
  Failure a <*> Success _ = Failure a
  Success _ <*> Failure b = Failure b
  Success f <*> Success x = Success (f x)

-- | Convert a 'Result'.
resultToEither :: Result a -> Either ConfigErrors a
resultToEither (Failure e) = Left e
resultToEither (Success a) = Right a

-- | What a decoder knows about where it is.
data DecodeContext = DecodeContext
  { ctxPath :: !Path
  , ctxProfile :: !Profile
  , ctxLossy :: !Bool
    -- ^ accept strings for numbers and booleans, and scalars for strings
  , ctxSources :: ![Metadata]
    -- ^ every provider consulted, for missing-field hints
  }

-- | A context at the root, for the given profile and sources.
rootContext :: Profile -> [Metadata] -> DecodeContext
rootContext p ms = DecodeContext root p False ms

-- | Decodes a t'Value' in a t'DecodeContext'. Accumulates errors (see the module header).
newtype Decoder a = Decoder {unDecoder :: DecodeContext -> Value -> Result a}
  deriving stock (Functor)

instance Applicative Decoder where
  pure a = Decoder $ \_ _ -> Success a
  Decoder f <*> Decoder g = Decoder $ \c v -> f c v <*> g c v

-- | Run a decoder.
runDecoder :: Decoder a -> DecodeContext -> Value -> Either ConfigErrors a
runDecoder d c v = resultToEither (unDecoder d c v)

-- | Run a decoder, keeping the accumulating 'Result'.
decodeWith :: Decoder a -> DecodeContext -> Value -> Result a
decodeWith = unDecoder

-- | Build a decoder from a function (for custom instances that need the
-- context, e.g. to report errors at a precise location).
mkDecoder :: (DecodeContext -> Value -> Result a) -> Decoder a
mkDecoder = Decoder

-- | Sequential composition. Short-circuits: errors from the second step
-- are only seen if the first succeeds.
andThen :: Decoder a -> (a -> Decoder b) -> Decoder b
andThen (Decoder f) k = Decoder $ \c v -> case f c v of
  Failure e -> Failure e
  Success a -> unDecoder (k a) c v

-- | Inspect the raw value before choosing a decoder.
withValue :: (Value -> Decoder a) -> Decoder a
withValue k = Decoder $ \c v -> unDecoder (k v) c v

-- | The key path of the value being decoded.
currentPath :: Decoder Path
currentPath = Decoder $ \c _ -> Success (ctxPath c)

errorAt :: DecodeContext -> Value -> ErrorKind -> ConfigErrors
errorAt c v k =
  let t = valueTag v
  in single
       ConfigError
         { errorKind = k
         , errorPath = ctxPath c
         , errorProfile = Just (fromMaybe (ctxProfile c) (tagProfile t))
         , errorMetadata = tagMeta t
         , errorPos = tagPos t
         , errorHints = []
         }

-- | Fail at the current value.
failWith :: ErrorKind -> Decoder a
failWith k = Decoder $ \c v -> Failure (errorAt c v k)

-- | Post-process a decoded value; a 'Left' becomes a 'Message' error
-- located at this value (its source, line and column).
validate :: (a -> Either Text b) -> Decoder a -> Decoder b
validate f (Decoder g) = Decoder $ \c v -> case g c v of
  Failure e -> Failure e
  Success a -> case f a of
    Left msg -> Failure (errorAt c v (Message msg))
    Right b -> Success b

typeError :: DecodeContext -> Value -> Text -> Result a
typeError c v expected = Failure (errorAt c v (InvalidType (describe (valueNode v)) expected))

valueError :: DecodeContext -> Value -> Text -> Result a
valueError c v expected = Failure (errorAt c v (InvalidValue (describe (valueNode v)) expected))

------------------------------------------------------------------------
-- The class

-- | Types that can be decoded from configuration. Derive it generically (@deriving anyclass FromConfig@ with a 'Generic' instance) or write 'fromConfig' with the combinators below.
class FromConfig a where
  fromConfig :: Decoder a
  default fromConfig :: (Generic a, GDecode (Rep a)) => Decoder a
  fromConfig = genericDecoder defaultOptions

  -- | The value to use when a record field of this type is absent.
  -- 'Nothing' (the default) makes the field required; 'Maybe' fields
  -- answer @Just Nothing@.
  missingValue :: Maybe a
  missingValue = Nothing

  -- | How to decode a list of this type. Exists so that t'String'
  -- decodes from a string rather than an array of characters.
  listDecoder :: Decoder [a]
  listDecoder = Vector.toList <$> arrayOf fromConfig

------------------------------------------------------------------------
-- Field access

-- | A required field of the current dictionary (or its
-- 'missingValue', for types that have one).
field :: forall a. FromConfig a => Text -> Decoder a
field name = fieldWith name fromConfig (missingValue @a)

-- | A field with an explicit decoder and an optional fallback.
fieldWith :: Text -> Decoder a -> Maybe a -> Decoder a
fieldWith name d missing = Decoder $ \c v -> case valueNode v of
  Dict m -> case Map.lookup name m of
    Just sub -> unDecoder d c {ctxPath = ctxPath c /. name} sub
    Nothing -> maybe (Failure (single (missingFieldError c name))) Success missing
  _ -> typeError c v "a dictionary"

-- | An optional field: 'Nothing' when absent or null.
optionalField :: FromConfig a => Text -> Decoder (Maybe a)
optionalField name = fieldWith name fromConfig (Just Nothing)

-- | A field with a default used when it is absent.
fieldOr :: FromConfig a => Text -> a -> Decoder a
fieldOr name def = fieldWith name fromConfig (Just def)

-- | The error for an absent required field, with a hint naming every
-- way it could have been supplied.
missingFieldError :: DecodeContext -> Text -> ConfigError
missingFieldError c name =
  let p = ctxPath c /. name
      prof = ctxProfile c
      ways = [ "`" <> interpolate m prof p <> "` in " <> metaName m <> maybe "" ((" " <>) . renderSource) (metaSource m)
             | m <- ctxSources c ]
      hint = case NE.nonEmpty ways of
        Nothing -> []
        Just (w :| []) -> ["no source sets it; it can be set as " <> w]
        Just ws -> ["no source sets it; it can be set as " <> Text.intercalate ", " (NE.init ws) <> ", or " <> NE.last ws]
  in ConfigError (MissingField name) p (Just prof) Nothing Nothing hint

------------------------------------------------------------------------
-- Containers

-- | Decode every element of an array (errors in different elements accumulate).
arrayOf :: Decoder a -> Decoder (Vector a)
arrayOf (Decoder d) = Decoder $ \c v -> case valueNode v of
  Array xs -> traverse (\(i, x) -> d c {ctxPath = ctxPath c /# i} x) (Vector.indexed xs)
  _ -> typeError c v "an array"

-- | Decode every value of a dictionary.
dictOf :: Decoder a -> Decoder (Map Text a)
dictOf (Decoder d) = Decoder $ \c v -> case valueNode v of
  Dict m -> Map.traverseWithKey (\k x -> d c {ctxPath = ctxPath c /. k} x) m
  _ -> typeError c v "a dictionary"

-- | Keys of decoded maps.
class FromConfigKey k where
  fromConfigKey :: Text -> Either Text k

instance FromConfigKey Text where fromConfigKey = Right
instance FromConfigKey [Char] where fromConfigKey = Right . Text.unpack

------------------------------------------------------------------------
-- Scalars

-- | Text: a string; or the original text of a value parsed from an
-- untyped source; or (lossy mode) any scalar rendered as text.
textual :: Decoder Text
textual = Decoder $ \c v -> case valueNode v of
  String t -> Success t
  n
    | Just r <- tagRaw (valueTag v) -> Success r
    | ctxLossy c, Just t <- scalarText n -> Success t
    | otherwise -> typeError c v "a string"

scalarText :: Node -> Maybe Text
scalarText = \case
  Bool b -> Just (if b then "true" else "false")
  Num (Finite s)
    | Sci.isInteger s, abs (Sci.base10Exponent s) < 64 -> Just (Text.pack (Sci.formatScientific Sci.Fixed (Just 0) s))
    | otherwise -> Just (Text.pack (show s))
  Num PosInfinity -> Just "inf"
  Num NegInfinity -> Just "-inf"
  Num NaN -> Just "nan"
  _ -> Nothing

-- | A number, accepting strings in lossy mode.
numberD :: Text -> Decoder Number
numberD expected = Decoder $ \c v -> case valueNode v of
  Num n -> Success n
  String t | ctxLossy c, Just n <- parseNumber (Text.strip t) -> Success n
  _ -> typeError c v expected

-- | Integral types with bounds, rejecting non-integers and
-- out-of-range values (never wrapping, never allocating huge integers).
boundedIntegral :: forall a. (Integral a, Bounded a, Show a) => Decoder a
boundedIntegral = decodeIntegral $ \s ->
  case Sci.toBoundedInteger s of
    Just x -> Right x
    Nothing -> Left ("an integer in the range " <> tshow (minBound :: a) <> ".." <> tshow (maxBound :: a))

decodeIntegral :: (Scientific -> Either Text a) -> Decoder a
decodeIntegral conv = Decoder $ \c v -> case unDecoder (numberD "an integer") c v of
  Failure e -> Failure e
  Success (Finite s)
    | not (Sci.isInteger s) -> valueError c v "an integer"
    | otherwise -> either (valueError c v) Success (conv s)
  Success _ -> valueError c v "a finite integer"

unboundedInteger :: Decoder Integer
unboundedInteger = decodeIntegral $ \s ->
  if tooManyDigits s
    then Left "an integer with at most ~4096 digits"
    else Right (truncate s)

-- | Does the integral value have more than about 4096 decimal digits?
-- Checks the coefficient as well as the exponent, so a long literal such
-- as a megabyte of digits is rejected like @1e5000@ is.
tooManyDigits :: Scientific -> Bool
tooManyDigits s =
  e > 4096
    || abs c >= digitsLimit
    || (e > 0 && abs c * 10 ^ e >= digitsLimit)
  where
    e = Sci.base10Exponent s
    c = Sci.coefficient s

digitsLimit :: Integer
digitsLimit = 10 ^ (4096 :: Int)

realFloatD :: RealFloat a => Decoder a
realFloatD = Decoder $ \c v -> case unDecoder (numberD "a number") c v of
  Failure e -> Failure e
  Success (Finite s)
    -- a finite literal that overflows the target type is an error, not a
    -- silent infinity
    | isInfinite x -> valueError c v "a number within the range of the type"
    | otherwise -> Success x
    where
      x = Sci.toRealFloat s
  Success PosInfinity -> Success (1 / 0)
  Success NegInfinity -> Success (-1 / 0)
  Success NaN -> Success (0 / 0)

instance FromConfig Value where fromConfig = Decoder $ \_ v -> Success v

instance FromConfig Text where fromConfig = textual

instance FromConfig Char where
  fromConfig = Decoder $ \c v -> case unDecoder textual c v of
    Success t
      | Just (ch, rest) <- Text.uncons t, Text.null rest -> Success ch
      | otherwise -> valueError c v "a single character"
    Failure e -> Failure e
  listDecoder = Text.unpack <$> textual

instance FromConfig Bool where
  fromConfig = Decoder $ \c v -> case valueNode v of
    Bool b -> Success b
    String t | ctxLossy c, Just b <- lossyBool t -> Success b
    Num (Finite s) | ctxLossy c, s == 0 -> Success False
                   | ctxLossy c, s == 1 -> Success True
    _ -> typeError c v "a boolean"
    where
      lossyBool t = case Text.toLower (Text.strip t) of
        x | x `elem` ["true", "yes", "on", "1"] -> Just True
          | x `elem` ["false", "no", "off", "0"] -> Just False
          | otherwise -> Nothing

instance FromConfig Int where fromConfig = boundedIntegral
instance FromConfig Int8 where fromConfig = boundedIntegral
instance FromConfig Int16 where fromConfig = boundedIntegral
instance FromConfig Int32 where fromConfig = boundedIntegral
instance FromConfig Int64 where fromConfig = boundedIntegral
instance FromConfig Word where fromConfig = boundedIntegral
instance FromConfig Word8 where fromConfig = boundedIntegral
instance FromConfig Word16 where fromConfig = boundedIntegral
instance FromConfig Word32 where fromConfig = boundedIntegral
instance FromConfig Word64 where fromConfig = boundedIntegral
instance FromConfig Integer where fromConfig = unboundedInteger

instance FromConfig Natural where
  fromConfig = Decoder $ \c v -> case unDecoder unboundedInteger c v of
    Success n | n >= 0 -> Success (fromInteger n)
              | otherwise -> valueError c v "a non-negative integer"
    Failure e -> Failure e

instance FromConfig Double where fromConfig = realFloatD
instance FromConfig Float where fromConfig = realFloatD

instance FromConfig Scientific where
  fromConfig = Decoder $ \c v -> case unDecoder (numberD "a number") c v of
    Success (Finite s) -> Success s
    Success _ -> valueError c v "a finite number"
    Failure e -> Failure e

instance FromConfig () where
  fromConfig = Decoder $ \c v -> case valueNode v of
    Null -> Success ()
    Dict m | Map.null m -> Success ()
    Array xs | Vector.null xs -> Success ()
    _ -> typeError c v "null"

instance FromConfig a => FromConfig (Maybe a) where
  fromConfig = Decoder $ \c v -> case valueNode v of
    Null -> Success Nothing
    _ -> Just <$> unDecoder fromConfig c v
  missingValue = Just Nothing

instance FromConfig a => FromConfig [a] where
  fromConfig = listDecoder

instance FromConfig a => FromConfig (NonEmpty a) where
  fromConfig = Decoder $ \c v -> case unDecoder (listDecoder @a) c v of
    Success (x : xs) -> Success (x :| xs)
    Success [] -> Failure (errorAt c v (InvalidLength 0 "a non-empty array"))
    Failure e -> Failure e

instance FromConfig a => FromConfig (Vector a) where fromConfig = arrayOf fromConfig

instance FromConfig a => FromConfig (Seq a) where
  fromConfig = Seq.fromList . Vector.toList <$> arrayOf fromConfig

instance (Ord a, FromConfig a) => FromConfig (Set a) where
  fromConfig = Set.fromList . Vector.toList <$> arrayOf fromConfig

instance (Ord k, FromConfigKey k, FromConfig a) => FromConfig (Map k a) where
  fromConfig = Decoder $ \c v -> case unDecoder (dictOf fromConfig) c v of
    Failure e -> Failure e
    Success m ->
      let conv (k, x) = case fromConfigKey k of
            Right k' -> Success (k', x)
            Left msg -> Failure (errorAt c {ctxPath = ctxPath c /. k} v (InvalidValue ("key `" <> k <> "`") msg))
      in Map.fromList <$> traverse conv (Map.toList m)

instance (FromConfig a, FromConfig b) => FromConfig (a, b) where
  fromConfig = tupleD 2 $ \c xs -> (,) <$> at c xs 0 <*> at c xs 1

instance (FromConfig a, FromConfig b, FromConfig c) => FromConfig (a, b, c) where
  fromConfig = tupleD 3 $ \c xs -> (,,) <$> at c xs 0 <*> at c xs 1 <*> at c xs 2

tupleD :: Int -> (DecodeContext -> Vector Value -> Result a) -> Decoder a
tupleD n k = Decoder $ \c v -> case valueNode v of
  Array xs
    | Vector.length xs == n -> k c xs
    | otherwise -> Failure (errorAt c v (InvalidLength (Vector.length xs) ("an array of length " <> tshow n)))
  _ -> typeError c v ("an array of length " <> tshow n)

at :: FromConfig a => DecodeContext -> Vector Value -> Int -> Result a
at c xs i = case xs Vector.!? i of
  Just x -> unDecoder fromConfig c {ctxPath = ctxPath c /# i} x
  Nothing -> Failure (single (atPath (ctxPath c /# i) (configError (InvalidLength i "more elements"))))

-- Time: ISO 8601 text (TOML date/time values arrive as such text).
isoD :: ISO8601 t => Text -> Decoder t
isoD what = Decoder $ \c v -> case unDecoder textual c v of
  Failure e -> Failure e
  Success t -> case iso8601ParseM (Text.unpack (Text.strip t)) of
    Just x -> Success x
    Nothing -> valueError c v ("an ISO 8601 " <> what)

instance FromConfig Day where fromConfig = isoD "date"
instance FromConfig TimeOfDay where fromConfig = isoD "time of day"
instance FromConfig LocalTime where fromConfig = isoD "local date-time"
instance FromConfig ZonedTime where fromConfig = isoD "date-time with offset"
instance FromConfig UTCTime where
  fromConfig = Decoder $ \c v -> case unDecoder textual c v of
    Failure e -> Failure e
    Success t ->
      let s = Text.unpack (Text.strip t)
      in case (iso8601ParseM s, iso8601ParseM s) of
           (Just z, _) -> Success (zonedTimeToUTC z)
           (_, Just u) -> Success u
           _ -> valueError c v "an ISO 8601 date-time with offset"

------------------------------------------------------------------------
-- Magic types

-- | A value together with its provenance: which provider, which
-- profile, which line and column, and its key path. The Haskell
-- counterpart of Rust figment's @Tagged\<T\>@.
data Tagged a = Tagged
  { taggedValue :: a
  , taggedTag :: Tag
  , taggedPath :: Path
  }
  deriving stock (Eq, Show, Functor)

-- | The provider the value came from.
taggedMetadata :: Tagged a -> Maybe Metadata
taggedMetadata = tagMeta . taggedTag

instance FromConfig a => FromConfig (Tagged a) where
  fromConfig = Decoder $ \c v -> (\a -> Tagged a (valueTag v) (ctxPath c)) <$> unDecoder fromConfig c v
  missingValue = (\a -> Tagged a untagged root) <$> missingValue

-- | A path written in a configuration file, remembered together with
-- the directory of that file, so that relative paths can be resolved
-- against the file that declared them rather than the working
-- directory (Rust figment's @RelativePathBuf@).
data RelativePath = RelativePath
  { relativeOriginal :: FilePath
  , relativeBase :: Maybe FilePath
    -- ^ directory of the declaring file, if it came from a file
  }
  deriving stock (Eq, Show)

-- | Resolve: absolute paths and paths not from a file are returned
-- as written; others are joined to the declaring file's directory.
relative :: RelativePath -> FilePath
relative (RelativePath p base)
  | isAbsolute p = p
  | otherwise = maybe p (</> p) base

instance FromConfig RelativePath where
  fromConfig = Decoder $ \c v -> case unDecoder textual c v of
    Failure e -> Failure e
    Success t ->
      let base = case tagMeta (valueTag v) >>= metaSource of
            Just (FileSource f) -> Just (takeDirectory f)
            _ -> Nothing
      in Success (RelativePath (Text.unpack t) base)

------------------------------------------------------------------------
-- Generic deriving

-- | Options for 'genericDecoder'.
data Options = Options
  { fieldLabelModifier :: String -> String
    -- ^ field name to configuration key
  , constructorTagModifier :: String -> String
    -- ^ constructor name to enum tag
  , rejectUnknownFields :: Bool
    -- ^ report keys the record does not expect (with suggestions)
  }

-- | Field and constructor names used as written; unknown keys allowed.
defaultOptions :: Options
defaultOptions = Options id id False

-- | @maxConnections@ to @max_connections@.
snakeCase :: String -> String
snakeCase = separate '_'

-- | @maxConnections@ to @max-connections@.
kebabCase :: String -> String
kebabCase = separate '-'

-- | Insert the separator at word boundaries: before an uppercase letter
-- that follows a lowercase letter or digit, and before the last capital of
-- an acronym that is followed by a lowercase letter. @dbURL@ becomes
-- @db_url@, @HTTPServer@ becomes @http_server@, @useHTTPS@ becomes
-- @use_https@.
separate :: Char -> String -> String
separate sep = go Nothing
  where
    go _ [] = []
    go prev (c : cs)
      | isUpper c = [sep | boundary] ++ toLower c : go (Just c) cs
      | otherwise = c : go (Just c) cs
      where
        boundary = case prev of
          Nothing -> False
          Just p
            | isLower p || isDigit p -> True
            | isUpper p -> case cs of
                (n : _) -> isLower n
                [] -> False
            | otherwise -> False

-- | Derive a decoder from a 'Generic' instance: records are dictionaries, enums are strings (matched case-insensitively), other sums are externally tagged (@{ Variant = payload }@), newtypes unwrap, positional constructors are arrays.
genericDecoder :: (Generic a, GDecode (Rep a)) => Options -> Decoder a
genericDecoder o = to <$> gdecode o

-- | Proxy for generic metadata, so that names are read without
-- conjuring values.
data P (m :: Meta) (f :: Type -> Type) a = P

-- | Generic machinery behind 'genericDecoder'.
class GDecode (f :: Type -> Type) where
  gdecode :: Options -> Decoder (f x)

data Ctor a = Ctor
  { ctorTag :: Text
  , ctorNullary :: Bool
  , ctorPayload :: Decoder a
  }
  deriving stock (Functor)

instance GCtors f => GDecode (D1 d f) where
  gdecode o = M1 <$> decodeCtors (gctors o)

class GCtors (f :: Type -> Type) where
  gctors :: Options -> [Ctor (f x)]

instance (GCtors f, GCtors g) => GCtors (f :+: g) where
  gctors o = map (fmap L1) (gctors o) ++ map (fmap R1) (gctors o)

instance (Constructor c, GFields f, GPositional f) => GCtors (C1 c f) where
  gctors o =
    [ Ctor
        { ctorTag = Text.pack (constructorTagModifier o (conName (P :: P c f ())))
        , ctorNullary = arity == 0
        , ctorPayload = M1 <$> payload
        }
    ]
    where
      arity = garity (Proxy :: Proxy f)
      payload
        | arity == 0 = Decoder $ \_ _ -> gposition o emptyCtx 0 Vector.empty
        | conIsRecord (P :: P c f ()) = recordD o (gfieldNames o (Proxy :: Proxy f)) (gfields o)
        | arity == 1 = case gunwrapped o of
            Just d -> d
            Nothing -> tupleD 1 (\c xs -> gposition o c 0 xs)
        | otherwise = tupleD arity (\c xs -> gposition o c 0 xs)
      emptyCtx = DecodeContext root Default False []

instance
  TypeError ('Text "figment: cannot derive FromConfig for an empty data type")
  => GCtors V1
  where
  gctors _ = []

decodeCtors :: [Ctor a] -> Decoder a
decodeCtors [c] | not (ctorNullary c) = ctorPayload c
decodeCtors [c] = Decoder $ \ctx v -> case valueNode v of
  -- a unit-like type accepts null, an empty container, or its own name
  Null -> unDecoder (ctorPayload c) ctx v
  Dict m | Map.null m -> unDecoder (ctorPayload c) ctx v
  _ -> unDecoder (decodeCtors' [c]) ctx v
decodeCtors cs = decodeCtors' cs

decodeCtors' :: [Ctor a] -> Decoder a
decodeCtors' cs = Decoder $ \c v ->
  let tags = map ctorTag cs
      byTag t = case filter ((== t) . ctorTag) cs of
        (x : _) -> Just x
        [] -> case filter ((== Text.toCaseFold t) . Text.toCaseFold . ctorTag) cs of
          [x] -> Just x
          _ -> Nothing
      unknown t = Failure (errorAt c v (UnknownVariant t tags))
  in case valueNode v of
       Dict m -> case Map.toList m of
         [(k, payload)] -> case byTag k of
           Just ctor -> unDecoder (ctorPayload ctor) c {ctxPath = ctxPath c /. k} payload
           Nothing -> Failure (errorAt c {ctxPath = ctxPath c /. k} payload (UnknownVariant k tags))
         _ -> valueError c v "a variant name, or a dictionary with exactly one key naming a variant"
       _ -> case unDecoder textual c v of
         Success t -> case byTag (Text.strip t) of
           Just ctor
             | ctorNullary ctor -> unDecoder (ctorPayload ctor) c v
             | otherwise -> valueError c v ("a dictionary `{ " <> ctorTag ctor <> " = ... }` (variant has fields)")
           Nothing -> unknown t
         Failure _ -> typeError c v ("one of " <> Text.intercalate ", " (map (\t -> "`" <> t <> "`") tags))

recordD :: Options -> [Text] -> (DecodeContext -> Dict -> Result a) -> Decoder a
recordD o known fields = Decoder $ \c v -> case valueNode v of
  Dict m ->
    let knownSet = Set.fromList known
        unknown = [kv | rejectUnknownFields o, kv@(k, _) <- Map.toList m, not (k `Set.member` knownSet)]
        unknownErrs =
          [ errorAt c {ctxPath = ctxPath c /. k} x (UnknownField k (suggestAt i k known))
          | (i, (k, x)) <- zip [0 ..] unknown
          ]
        unknownR = case NE.nonEmpty unknownErrs of
          Nothing -> Success ()
          Just es -> Failure (sconcat es)
     in unknownR *> fields c m
  _ -> typeError c v "a dictionary"

-- | Candidates within a small edit distance, best first.
--
-- Suggestions are a courtesy, so their cost is bounded: keys longer than
-- 'maxSuggestLength' get none, and candidates whose length differs too much
-- to be within range are skipped without computing a distance.
suggest :: Text -> [Text] -> [Text]
suggest k known
  | Text.compareLength k maxSuggestLength == GT = []
  | otherwise = map snd (take 3 (sortOn fst scored))
  where
    lk = Text.length k
    scored =
      [ (d, cand)
      | cand <- known
      , let lc = Text.length cand
            limit = max 1 (lc `div` 3)
      , abs (lk - lc) <= limit -- the distance is at least the length difference
      , let d = editDistance k cand
      , d <= limit
      ]

-- | 'suggest' for the @i@th unknown key of a record (counting from 0). Only
-- the first 'suggestionBudget' unknown keys receive suggestions, so a
-- document full of typos costs a bounded amount of work to report.
suggestAt :: Int -> Text -> [Text] -> [Text]
suggestAt i k known
  | i >= suggestionBudget = []
  | otherwise = suggest k known

-- | Longest key (in characters) that is compared against field names.
maxSuggestLength :: Int
maxSuggestLength = 64

-- | How many unknown keys per record are given suggestions.
suggestionBudget :: Int
suggestionBudget = 32

-- | Optimal string alignment distance (Levenshtein plus adjacent
-- transpositions, so @hots@ is one edit from @host@). Runs in
-- O(length a * length b) time and O(length b) space: one unboxed row at a
-- time, never the whole table.
editDistance :: Text -> Text -> Int
editDistance a b
  | la == 0 = lb
  | lb == 0 = la
  | otherwise = U.last (snd (foldl' step (row0, row0) [1 .. la]))
  where
    av = U.fromList (Text.unpack a) :: U.Vector Char
    bv = U.fromList (Text.unpack b) :: U.Vector Char
    la = U.length av
    lb = U.length bv
    row0 = U.enumFromN 0 (lb + 1) :: U.Vector Int

    -- (row i-2, row i-1) -> (row i-1, row i)
    step (!prev2, !prev1) i = (prev1, cur)
      where
        ca = av U.! (i - 1)
        cur = U.constructN (lb + 1) cell
        cell built
          | j == 0 = i
          | i > 1 && j > 1 && ca == bv U.! (j - 2) && av U.! (i - 2) == cb =
              min base (prev2 U.! (j - 2) + 1)
          | otherwise = base
          where
            j = U.length built
            cb = bv U.! (j - 1)
            cost = if ca == cb then 0 else 1
            base = minimum [prev1 U.! j + 1, built U.! (j - 1) + 1, prev1 U.! (j - 1) + cost]

class GFields (f :: Type -> Type) where
  gfields :: Options -> DecodeContext -> Dict -> Result (f x)
  gfieldNames :: Options -> Proxy f -> [Text]

instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields o c m = (:*:) <$> gfields o c m <*> gfields o c m
  gfieldNames o _ = gfieldNames o (Proxy :: Proxy f) ++ gfieldNames o (Proxy :: Proxy g)

instance GFields U1 where
  gfields _ _ _ = Success U1
  gfieldNames _ _ = []

instance (Selector s, FromConfig a) => GFields (S1 s (Rec0 a)) where
  gfields o c m =
    let name = Text.pack (fieldLabelModifier o (selName (P :: P s (Rec0 a) ())))
    in case Map.lookup name m of
         Just v -> M1 . K1 <$> unDecoder fromConfig c {ctxPath = ctxPath c /. name} v
         Nothing -> case missingValue @a of
           Just d -> Success (M1 (K1 d))
           Nothing -> Failure (single (missingFieldError c name))
  gfieldNames o _ = [Text.pack (fieldLabelModifier o (selName (P :: P s (Rec0 a) ())))]

class GPositional (f :: Type -> Type) where
  garity :: Proxy f -> Int
  gposition :: Options -> DecodeContext -> Int -> Vector Value -> Result (f x)
  gunwrapped :: Options -> Maybe (Decoder (f x))
  gunwrapped _ = Nothing

instance (GPositional f, GPositional g) => GPositional (f :*: g) where
  garity _ = garity (Proxy :: Proxy f) + garity (Proxy :: Proxy g)
  gposition o c i xs = (:*:) <$> gposition o c i xs <*> gposition o c (i + garity (Proxy :: Proxy f)) xs

instance GPositional U1 where
  garity _ = 0
  gposition _ _ _ _ = Success U1

instance FromConfig a => GPositional (S1 s (Rec0 a)) where
  garity _ = 1
  gposition _ c i xs = M1 . K1 <$> at c xs i
  gunwrapped _ = Just (M1 . K1 <$> fromConfig)

tshow :: Show a => a -> Text
tshow = Text.pack . show

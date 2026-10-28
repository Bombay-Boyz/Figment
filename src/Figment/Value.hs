-- | The configuration value tree. Every node carries a t'Tag' recording
-- which provider and profile it came from, where in the source text it
-- was written, and (for values parsed from untyped text such as
-- environment variables) the original text.
module Figment.Value
  ( -- * Values
    Value (..)
  , Node (..)
  , Number (..)
  , Dict
    -- * Tags
  , Tag (..)
  , untagged
  , retag
  , setTagMeta
    -- * Construction
  , mkValue
  , string
  , bool
  , number
  , null
  , array
  , dict
  , emptyDict
    -- * Inspection
  , lookupPath
  , describe
  , kindOf
  , stripTags
  , numberFromScientific
  ) where

import Prelude hiding (null)

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import GHC.Generics (Generic)

import Figment.Metadata
import Figment.Path
import Figment.Profile

-- | Numbers. Finite values are exact ('Scientific'), so a 30-digit
-- decimal in a JSON file survives intact; TOML and YAML also allow
-- infinities and NaN, which are represented explicitly rather than
-- being smuggled through as nonsense 'Scientific's.
data Number
  = Finite !Scientific
  | PosInfinity
  | NegInfinity
  | NaN
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Provenance attached to a value.
data Tag = Tag
  { tagMeta :: !(Maybe Metadata)
  , tagProfile :: !(Maybe Profile)
  , tagPos :: !(Maybe Pos)
  , tagRaw :: !(Maybe Text)
    -- ^ the original text, for values parsed out of untyped text
    -- (environment variables, CLI arguments). Lets a 'Text' field
    -- receive @007@ verbatim even though it also parses as a number.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A tag with no provenance.
untagged :: Tag
untagged = Tag Nothing Nothing Nothing Nothing

-- | A dictionary. Keys are kept sorted (Rust figment's default ordering).
type Dict = Map Text Value

-- | The shape of a value.
data Node
  = Null
  | Bool !Bool
  | Num !Number
  | String !Text
  | Array !(Vector Value)
  | Dict !Dict
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A tagged configuration value.
data Value = Value
  { valueTag :: !Tag
  , valueNode :: !Node
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | An untagged value.
mkValue :: Node -> Value
mkValue = Value untagged

-- | An untagged string.
string :: Text -> Value
string = mkValue . String

-- | An untagged boolean.
bool :: Bool -> Value
bool = mkValue . Bool

-- | An untagged finite number.
number :: Scientific -> Value
number = mkValue . Num . Finite

-- | The untagged null.
null :: Value
null = mkValue Null

-- | An untagged array.
array :: [Value] -> Value
array = mkValue . Array . Vector.fromList

-- | An untagged dictionary.
dict :: [(Text, Value)] -> Value
dict = mkValue . Dict . Map.fromList

-- | The empty dictionary.
emptyDict :: Value
emptyDict = mkValue (Dict Map.empty)

-- | A finite number.
numberFromScientific :: Scientific -> Number
numberFromScientific = Finite

-- | Rebuild a tree, applying a function to every tag.
--
-- The rebuild is /strict/: when the result is forced, every node has been
-- built. ('fmap' over a 'Data.Map.Strict.Map' or a boxed 'Vector.Vector' is
-- lazy in the elements, and would leave a thunk per child that keeps the
-- original tree alive until it is touched.)
mapTags :: (Tag -> Tag) -> Value -> Value
mapTags f = go
  where
    go (Value t n) = Value (f t) (goNode n)
    goNode (Array xs) = Array (mapVector' go xs)
    goNode (Dict d) = Dict (Map.map go d)
    goNode n = n

-- | 'Vector.map' that evaluates every element before the vector is
-- returned.
mapVector' :: (a -> b) -> Vector a -> Vector b
mapVector' f v = Vector.create $ do
  mv <- MVector.new (Vector.length v)
  Vector.iforM_ v $ \i x -> let y = f x in y `seq` MVector.write mv i y
  pure mv

-- | Fill in missing provenance (never overwrite: values coming from a
-- nested t'Figment.Core.Figment' keep their original source).
retag :: Metadata -> Profile -> Value -> Value
retag m p = mapTags fill
  where
    jm = Just m
    jp = Just p
    fill t = t
      { tagMeta = case tagMeta t of Nothing -> jm; old -> old
      , tagProfile = case tagProfile t of Nothing -> jp; old -> old
      }

-- | Overwrite the metadata of every node (used by providers that learn
-- their final metadata, e.g. a resolved file path, only when reading).
setTagMeta :: Metadata -> Value -> Value
setTagMeta m = mapTags (\t -> t {tagMeta = jm})
  where
    jm = Just m

-- | Follow a path.
lookupPath :: Path -> Value -> Maybe Value
lookupPath p v0 = foldl' step (Just v0) (segments p)
  where
    step Nothing _ = Nothing
    step (Just (Value _ (Dict d))) (Key k) = Map.lookup k d
    step (Just (Value _ (Array xs))) (Index i) = xs Vector.!? i
    step _ _ = Nothing

-- | A short description of a node's kind, for error messages.
kindOf :: Node -> Text
kindOf = \case
  Null -> "null"
  Bool _ -> "a boolean"
  Num (Finite s) | Sci.isInteger s -> "an integer"
  Num _ -> "a number"
  String _ -> "a string"
  Array _ -> "an array"
  Dict _ -> "a dictionary"

-- | Kind plus a preview of the value, e.g. @string "abc"@.
describe :: Node -> Text
describe = \case
  Null -> "null"
  Bool b -> "boolean " <> (if b then "true" else "false")
  Num n -> case n of
    Finite s
      | Sci.isInteger s, abs (Sci.base10Exponent s) < 64 ->
          "integer " <> Text.pack (Sci.formatScientific Sci.Fixed (Just 0) s)
      | otherwise -> "number " <> Text.pack (show s)
    PosInfinity -> "number inf"
    NegInfinity -> "number -inf"
    NaN -> "number nan"
  String t -> "string " <> Text.pack (show (preview t))
  Array xs -> "an array of length " <> Text.pack (show (Vector.length xs))
  Dict _ -> "a dictionary"
  where
    preview t
      | Text.compareLength t 40 == GT = Text.take 40 t <> "..."
      | otherwise = t

-- | Remove all provenance (for comparing data only).
stripTags :: Value -> Value
stripTags = mapTags (const untagged)

-- | Key paths into a configuration tree.
--
-- A t'Path' is a sequence of 'Segment's: either a dictionary 'Key' or an
-- array 'Index'. Keeping the two apart (rather than encoding indices as
-- text) means key @"0"@ and index @0@ can never be confused, and error
-- messages can render @servers[2].port@ faithfully.
module Figment.Path
  ( Segment (..)
  , Path
  , root
  , isRoot
  , segments
  , fromSegments
  , (/.)
  , (/#)
  , keyPath
  , parsePath
  , renderPath
  , renderSegment
  ) where

import Control.DeepSeq (NFData)
import Data.Char (isAlphaNum, isDigit)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- | One step into a configuration tree.
data Segment
  = Key !Text   -- ^ a dictionary key
  | Index !Int  -- ^ a zero-based array index
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A location in a configuration tree, from the root. Appending is
-- O(1) ('Seq' snoc), so building paths while descending is linear.
newtype Path = Path (Seq Segment)
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show Path where
  showsPrec d p = showParen (d > 10) $
    showString "keyPath " . shows (renderPath p)

instance Semigroup Path where
  Path a <> Path b = Path (a <> b)

instance Monoid Path where
  mempty = root

-- | The empty path: the top of the tree.
root :: Path
root = Path Seq.empty

-- | Is this the root path?
isRoot :: Path -> Bool
isRoot (Path s) = Seq.null s

-- | The segments of a path, from the root.
segments :: Path -> [Segment]
segments (Path s) = toList s

-- | Build a path from segments.
fromSegments :: [Segment] -> Path
fromSegments = Path . Seq.fromList

-- | Extend a path with a key.
(/.) :: Path -> Text -> Path
Path s /. k = Path (s |> Key k)
infixl 5 /.

-- | Extend a path with an index.
(/#) :: Path -> Int -> Path
Path s /# i = Path (s |> Index i)
infixl 5 /#

-- | A dotted key path, as used by Rust figment: @"db.host"@ is
-- @[Key "db", Key "host"]@. Total: empty segments are kept as empty keys
-- (so @keyPath ""@ is the single empty key, never an error). Use
-- 'parsePath' to read the full rendered syntax including indices and
-- quoted keys.
keyPath :: Text -> Path
keyPath t = fromSegments (map Key (Text.splitOn "." t))

-- | Render a path: keys joined with @.@, indices as @[n]@, and keys that
-- are not plain identifiers double-quoted. The root renders as the empty
-- text. @parsePath . renderPath == Right@.
renderPath :: Path -> Text
renderPath (Path s) = go True (toList s)
  where
    go _ [] = Text.empty
    go first (seg : rest) = piece first seg <> go False rest
    piece _ (Index i) = "[" <> Text.pack (show i) <> "]"
    piece first (Key k) = (if first then "" else ".") <> renderKey k

-- | Render a single segment on its own.
renderSegment :: Segment -> Text
renderSegment (Index i) = "[" <> Text.pack (show i) <> "]"
renderSegment (Key k) = renderKey k

renderKey :: Text -> Text
renderKey k
  | not (Text.null k) && Text.all plain k = k
  | otherwise = "\"" <> Text.concatMap esc k <> "\""
  where
    plain c = isAlphaNum c || c == '_' || c == '-'
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c = Text.singleton c

-- | Parse the syntax produced by 'renderPath'.
parsePath :: Text -> Either Text Path
parsePath t0
  | Text.null t0 = Right root
  | otherwise = fromSegments . reverse <$> start [] t0
  where
    start acc t = do
      (seg, rest) <- keySeg t
      more (seg : acc) rest

    more acc t = case Text.uncons t of
      Nothing -> Right acc
      Just ('.', rest) -> do
        (seg, rest') <- keySeg rest
        more (seg : acc) rest'
      Just ('[', rest) -> do
        let (ds, rest') = Text.span isDigit rest
        case Text.uncons rest' of
          Just (']', rest'') | not (Text.null ds) ->
            more (Index (read (Text.unpack ds)) : acc) rest''
          _ -> Left ("malformed index in path " <> quoted t0)
      Just (c, _) -> Left ("unexpected " <> Text.pack (show c) <> " in path " <> quoted t0)

    keySeg t = case Text.uncons t of
      Just ('[', _) -> idxFirst t
      Just ('"', rest) -> quotedKey "" rest
      _ ->
        let (k, rest) = Text.span (\c -> c /= '.' && c /= '[') t
        in if Text.null k
             then Left ("empty key in path " <> quoted t0)
             else Right (Key k, rest)

    idxFirst t = case Text.uncons t of
      Just ('[', rest) ->
        let (ds, rest') = Text.span isDigit rest
        in case Text.uncons rest' of
             Just (']', rest'') | not (Text.null ds) -> Right (Index (read (Text.unpack ds)), rest'')
             _ -> Left ("malformed index in path " <> quoted t0)
      _ -> Left ("malformed path " <> quoted t0)

    quotedKey acc t = case Text.uncons t of
      Nothing -> Left ("unterminated quoted key in path " <> quoted t0)
      Just ('"', rest) -> Right (Key acc, rest)
      Just ('\\', rest) -> case Text.uncons rest of
        Just (c, rest') -> quotedKey (Text.snoc acc c) rest'
        Nothing -> Left ("dangling escape in path " <> quoted t0)
      Just (c, rest) -> quotedKey (Text.snoc acc c) rest

    quoted x = "`" <> x <> "`"

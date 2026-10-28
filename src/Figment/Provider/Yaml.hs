-- | YAML, read directly from libyaml's event stream.
--
-- * YAML 1.2 core schema: only @true@/@false@ are booleans, so @no@,
--   @on@ and @NO@ (the \"Norway problem\") stay strings.
-- * Every value carries its line and column.
-- * Anchors and aliases are supported, and alias expansion is charged
--   against the node limit, which defeats \"billion laughs\".
-- * Merge keys (@<<: *base@) are supported; explicit keys win.
-- * Duplicate keys are errors; multiple documents are rejected.
module Figment.Provider.Yaml
  ( yaml
  , parseYaml
  ) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Conduit (ConduitT, await, runConduitRes, (.|))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import qualified Text.Libyaml as Y

import Figment.Error
import Figment.Internal.Budget (checkBytes)
import Figment.Internal.Number (integerFromDigits)
import Figment.Internal.ValueParser (parseNumber)
import Figment.Limits
import Figment.Metadata (Pos (..))
import Figment.Path
import Figment.Provider.Format (Format (..))
import Figment.Value hiding (null)

-- | The YAML format.
yaml :: Format
yaml = Format "YAML" parseYaml

-- | Parse a YAML document, enforcing the limits.
parseYaml :: Limits -> ByteString -> IO (Either ConfigError Value)
parseYaml lim bs = case checkBytes lim bs of
  Left e -> pure (Left e)
  Right () -> do
    r <- try (runConduitRes (Y.decodeMarked bs .| collectEvents lim))
    pure $ case r of
      Left (Y.YamlException msg) -> Left (configError (SyntaxError (Text.pack msg)))
      Left (Y.YamlParseException problem ctx mark) ->
        Left (withPos (Just (markPos mark))
          (configError (SyntaxError (Text.pack (problem ++ (if null ctx then "" else " " ++ ctx))))))
      Right (Left e) -> Left e
      Right (Right []) -> Right emptyDict
      Right (Right events) -> fst <$> runY document (YState events Map.empty (maxNodes lim)) lim

-- | Gather the events of the stream, but give up the moment the stream is
-- known to break a limit, rather than materialising all of it first.
--
-- * nesting is counted from the start/end events, so a hostile
--   @[[[[...@ is rejected at depth 'maxDepth' + 1, however long it is;
-- * every value the document builds is a node charged against 'maxNodes',
--   and each event is a value, a key (at most one per value) or the end
--   of a container (at most one per container), so a document within the
--   node limit has at most @3 * maxNodes@ events (plus the stream and
--   document framing). More than that cannot be within the limit.
--
-- Together with the size limit this bounds the memory held here, whatever
-- the input.
collectEvents
  :: Monad m
  => Limits
  -> ConduitT Y.MarkedEvent o m (Either ConfigError [Y.MarkedEvent])
collectEvents lim = go (0 :: Int) (0 :: Int) []
  where
    cap
      | maxNodes lim > (maxBound - 16) `div` 3 = maxBound
      | otherwise = 3 * maxNodes lim + 16
    go !depth !count acc = do
      me <- await
      case me of
        Nothing -> pure (Right (reverse acc))
        Just ev@(Y.MarkedEvent e start _)
          | count >= cap ->
              pure (Left (withPos (Just (markPos start)) (configError (TooManyNodes (maxNodes lim)))))
          | depth' > maxDepth lim ->
              pure (Left (withPos (Just (markPos start)) (configError (DepthExceeded (maxDepth lim)))))
          | otherwise -> go depth' (count + 1) (ev : acc)
          where
            depth' = case e of
              Y.EventSequenceStart {} -> depth + 1
              Y.EventMappingStart {} -> depth + 1
              Y.EventSequenceEnd -> depth - 1
              Y.EventMappingEnd -> depth - 1
              _ -> depth

markPos :: Y.YamlMark -> Pos
markPos m = Pos (Y.yamlLine m + 1) (Y.yamlColumn m + 1)

------------------------------------------------------------------------
-- A small state/error monad over the event list

data YState = YState
  { ysEvents :: [Y.MarkedEvent]
  , ysAnchors :: Map String Anchored
  , ysBudget :: !Int
  }

-- | An anchored subtree, with its expanded size and container height.
data Anchored = Anchored Value !Int !Int

newtype YP a = YP {runY :: YState -> Limits -> Either ConfigError (a, YState)}

instance Functor YP where
  fmap f (YP g) = YP $ \s l -> fmap (\(a, s') -> (f a, s')) (g s l)

instance Applicative YP where
  pure a = YP $ \s _ -> Right (a, s)
  YP f <*> YP g = YP $ \s l -> case f s l of
    Left e -> Left e
    Right (h, s') -> fmap (\(a, s'') -> (h a, s'')) (g s' l)

instance Monad YP where
  YP g >>= k = YP $ \s l -> case g s l of
    Left e -> Left e
    Right (a, s') -> runY (k a) s' l

failY :: ConfigError -> YP a
failY e = YP $ \_ _ -> Left e

next :: YP (Maybe Y.MarkedEvent)
next = YP $ \s _ -> case ysEvents s of
  [] -> Right (Nothing, s)
  (e : es) -> Right (Just e, s {ysEvents = es})

peek :: YP (Maybe Y.Event)
peek = YP $ \s _ -> Right (Y.yamlEvent <$> safeHead (ysEvents s), s)
  where
    safeHead (x : _) = Just x
    safeHead [] = Nothing

spendY :: Int -> Path -> Pos -> YP ()
spendY k p at = YP $ \s l ->
  let b = ysBudget s - k
  in if b < 0
       then Left (withPos (Just at) (atPath p (configError (TooManyNodes (maxNodes l)))))
       else Right ((), s {ysBudget = b})

depthCheck :: Int -> Path -> Pos -> YP ()
depthCheck d p at = YP $ \s l ->
  if d > maxDepth l
    then Left (withPos (Just at) (atPath p (configError (DepthExceeded (maxDepth l)))))
    else Right ((), s)

remember :: Y.Anchor -> Anchored -> YP ()
remember Nothing _ = pure ()
remember (Just name) a = YP $ \s _ -> Right ((), s {ysAnchors = Map.insert name a (ysAnchors s)})

recall :: String -> YP (Maybe Anchored)
recall name = YP $ \s _ -> Right (Map.lookup name (ysAnchors s), s)

syntaxAt :: Path -> Pos -> Text -> YP a
syntaxAt p at msg = failY (withPos (Just at) (atPath p (configError (SyntaxError msg))))

------------------------------------------------------------------------
-- Grammar

document :: YP Value
document = do
  _ <- expectStart
  ev <- peek
  case ev of
    Just Y.EventStreamEnd -> pure emptyDict
    Just Y.EventDocumentStart -> do
      _ <- next
      Anchored v _ _ <- node 1 root
      end <- next
      case Y.yamlEvent <$> end of
        Just Y.EventDocumentEnd -> pure ()
        _ -> failY (configError (SyntaxError "malformed YAML document"))
      rest <- peek
      case rest of
        Just Y.EventDocumentStart ->
          failY (configError (SyntaxError "multiple YAML documents are not supported in a configuration file"))
        _ -> pure v
    _ -> failY (configError (SyntaxError "malformed YAML stream"))
  where
    expectStart = next

-- | Parse one node at nesting level @d@ (root = 1). Returns the value
-- with its expanded size and container height.
node :: Int -> Path -> YP Anchored
node d p = do
  me <- next
  case me of
    Nothing -> failY (atPath p (configError (SyntaxError "unexpected end of YAML input")))
    Just (Y.MarkedEvent ev start _) -> do
      let at = markPos start
      case ev of
        Y.EventScalar bytes tag style anchor -> do
          spendY 1 p at
          n <- scalar p at bytes tag style
          let a = Anchored (Value untagged {tagPos = Just at} n) 1 0
          remember anchor a
          pure a
        Y.EventSequenceStart _ _ anchor -> do
          spendY 1 p at
          depthCheck d p at
          (items, size, height) <- sequenceItems 0 [] 1 0
          let a = Anchored (Value untagged {tagPos = Just at} (Array (Vector.fromList items))) size (height + 1)
          remember anchor a
          pure a
        Y.EventMappingStart _ _ anchor -> do
          spendY 1 p at
          depthCheck d p at
          (m, size, height) <- mappingPairs Map.empty [] False 1 0
          let a = Anchored (Value untagged {tagPos = Just at} (Dict m)) size (height + 1)
          remember anchor a
          pure a
        Y.EventAlias name -> do
          found <- recall name
          case found of
            Nothing -> syntaxAt p at ("unknown alias `*" <> Text.pack name <> "`")
            Just a@(Anchored _ size height) -> do
              spendY size p at
              if height > 0 then depthCheck (d + height - 1) p at else pure ()
              pure a
        _ -> syntaxAt p at "unexpected YAML event"
  where
    sequenceItems :: Int -> [Value] -> Int -> Int -> YP ([Value], Int, Int)
    sequenceItems i acc size height = do
      ev <- peek
      case ev of
        Just Y.EventSequenceEnd -> next >> pure (reverse acc, size, height)
        _ -> do
          Anchored v s h <- node (d + 1) (p /# i)
          sequenceItems (i + 1) (v : acc) (size + s) (max height h)

    -- explicit pairs, then merge-key sources (applied underneath). The Bool
    -- records that a merge key has been seen: a mapping may have only one,
    -- like any other key.
    mappingPairs :: Dict -> [Dict] -> Bool -> Int -> Int -> YP (Dict, Int, Int)
    mappingPairs acc merges seenMerge size height = do
      me <- next
      case me of
        Just (Y.MarkedEvent Y.EventMappingEnd _ _) ->
          pure (Map.union acc (Map.unions merges), size, height)
        Just (Y.MarkedEvent (Y.EventScalar bytes _ style _) kstart _) -> do
          let kat = markPos kstart
          key <- decodeText p kat bytes
          if key == "<<" && style `elem` [Y.Plain, Y.PlainNoTag, Y.Any]
            then
              if seenMerge
                then failY (withPos (Just kat) (atPath (p /. key) (configError (DuplicateKey key))))
                else do
                  Anchored v s h <- node (d + 1) (p /. key)
                  srcs <- mergeSources kat v
                  mappingPairs acc srcs True (size + s) (max height (h - 1))
            else
              if Map.member key acc
                then failY (withPos (Just kat) (atPath (p /. key) (configError (DuplicateKey key))))
                else do
                  Anchored v s h <- node (d + 1) (p /. key)
                  mappingPairs (Map.insert key v acc) merges seenMerge (size + s) (max height h)
        Just (Y.MarkedEvent _ kstart _) ->
          syntaxAt p (markPos kstart) "only scalar mapping keys are supported"
        Nothing -> failY (atPath p (configError (SyntaxError "unexpected end of YAML input")))

    mergeSources kat v = case valueNode v of
      Dict m -> pure [m]
      Array xs | all isDict (Vector.toList xs) -> pure [m | Value _ (Dict m) <- Vector.toList xs]
      _ -> syntaxAt (p /. "<<") kat "a merge key (`<<`) needs a mapping or a list of mappings"
    isDict (Value _ (Dict _)) = True
    isDict _ = False

decodeText :: Path -> Pos -> ByteString -> YP Text
decodeText p at bytes = case TE.decodeUtf8' bytes of
  Right t -> pure t
  Left _ -> syntaxAt p at "invalid UTF-8 in YAML scalar"

-- | Resolve a scalar under the YAML 1.2 core schema.
scalar :: Path -> Pos -> ByteString -> Y.Tag -> Y.Style -> YP Node
scalar p at bytes tag style = do
  t <- decodeText p at bytes
  let quoted = style `elem` [Y.SingleQuoted, Y.DoubleQuoted, Y.Literal, Y.Folded]
      bad what = syntaxAt p at ("`" <> t <> "` is not a valid " <> what)
  case tag of
    Y.StrTag -> pure (String t)
    Y.NullTag -> pure Null
    Y.BoolTag -> maybe (bad "boolean") (pure . Bool) (coreBool t)
    Y.IntTag -> maybe (bad "integer") (pure . Num) (coreInt t)
    Y.FloatTag -> maybe (bad "float") (pure . Num) (coreFloat t <|> coreInt t)
    _ | quoted -> pure (String t)
      | otherwise -> pure (resolve t)
  where
    resolve t
      | t `elem` ["", "~", "null", "Null", "NULL"] = Null
      | Just b <- coreBool t = Bool b
      | Just n <- coreInt t = Num n
      | Just n <- coreFloat t = Num n
      | otherwise = String t

coreBool :: Text -> Maybe Bool
coreBool t
  | t `elem` ["true", "True", "TRUE"] = Just True
  | t `elem` ["false", "False", "FALSE"] = Just False
  | otherwise = Nothing

coreInt :: Text -> Maybe Number
coreInt t
  | Just r <- Text.stripPrefix "0x" t, not (Text.null r), Text.all isHexDigit r =
      Just (Finite (fromInteger (integerFromDigits 16 r)))
  | Just r <- Text.stripPrefix "0o" t, not (Text.null r), Text.all isOctDigit r =
      Just (Finite (fromInteger (integerFromDigits 8 r)))
  | otherwise =
      let body = maybe t id (Text.stripPrefix "-" t <> Text.stripPrefix "+" t)
      in if not (Text.null body) && Text.all isDigit body then parseNumber t else Nothing


coreFloat :: Text -> Maybe Number
coreFloat t
  | t `elem` [".inf", ".Inf", ".INF", "+.inf", "+.Inf", "+.INF"] = Just PosInfinity
  | t `elem` ["-.inf", "-.Inf", "-.INF"] = Just NegInfinity
  | t `elem` [".nan", ".NaN", ".NAN"] = Just NaN
  | Text.any isDigit t, Text.all (`elem` ("0123456789+-.eE" :: String)) t =
      parseNumber (fixLeadingDot t)
  | otherwise = Nothing
  where
    fixLeadingDot x = case Text.uncons x of
      Just ('.', _) -> "0" <> x
      Just (s, r) | s `elem` ("+-" :: String), Just ('.', _) <- Text.uncons r -> Text.singleton s <> "0" <> r
      _ -> x

-- | Parsing untyped text (environment variables, CLI arguments) into
-- structured values, with syntax resembling TOML (as Rust figment's
-- @Env@ does):
--
-- * @true@ / @false@ are booleans
-- * integers and decimals (@-12@, @3.5@, @1e3@) are exact numbers;
--   @inf@, @-inf@, @nan@ are the non-finite numbers
-- * @\"double\"@ and @'single'@ quoted text is a string (quotes removed)
-- * @[a, b, c]@ is an array and @{k = v, k2 = v2}@ a dictionary; their
--   elements follow the same rules, and bare words inside them are
--   strings
-- * anything else, including malformed structured text, is the string
--   itself, verbatim
--
-- The top-level result always records the original text in 'tagRaw',
-- so a 'Text' field still receives @007@ rather than @7@.
module Figment.Internal.ValueParser
  ( parseValue
  , parseNumber
  ) where

import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as Map
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Figment.Error
import Figment.Limits
import Figment.Value

-- | Parse one untyped text value. Fails only when structured text is
-- nested beyond the depth limit; any syntax problem falls back to the
-- string.
parseValue :: Limits -> Text -> Either ConfigError Value
parseValue lim raw = case top of
  Left (Limit e) -> Left e
  Left NoParse -> Right (rawTagged (String raw))
  Right n -> Right (rawTagged n)
  where
    trimmed = Text.strip raw
    rawTagged n = Value untagged {tagRaw = Just raw} n
    top
      | Text.null trimmed = Right (String raw)
      | otherwise = case Text.head trimmed of
          c | c == '[' || c == '{' || c == '"' || c == '\'' ->
                case structured lim 1 trimmed of
                  Right (v, rest) | Text.null (Text.strip rest) -> Right (valueNode v)
                  Right _ -> Left NoParse
                  Left e -> Left e
          _ -> Right (scalarNode raw trimmed)

data Failure = NoParse | Limit ConfigError

-- | A bare scalar: boolean, number, or the text itself.
scalarNode :: Text -> Text -> Node
scalarNode original t
  | t == "true" = Bool True
  | t == "false" = Bool False
  | Just n <- parseNumber t = Num n
  | otherwise = String original

type P a = Either Failure (a, Text)

structured :: Limits -> Int -> Text -> P Value
structured lim depth t0 = case Text.uncons t of
  Just ('[', rest) -> do
    guardDepth
    (xs, rest') <- elements rest
    pure (mkValue (Array (Vector.fromList xs)), rest')
  Just ('{', rest) -> do
    guardDepth
    (kvs, rest') <- pairs rest
    pure (mkValue (Dict (Map.fromList kvs)), rest')
  Just ('"', rest) -> first' string (quotedDouble "" rest)
  Just ('\'', rest) -> first' string (quotedSingle rest)
  Just _ ->
    let (tok, rest) = Text.break (`elem` (",]}" :: String)) t
        tok' = Text.strip tok
    in if Text.null tok'
         then Left NoParse
         else Right (mkValue (scalarNode tok' tok'), rest)
  Nothing -> Left NoParse
  where
    t = Text.stripStart t0
    guardDepth
      | depth > maxDepth lim = Left (Limit (configError (DepthExceeded (maxDepth lim))))
      | otherwise = Right ()

    elements s0 =
      let s = Text.stripStart s0
      in case Text.uncons s of
           Just (']', rest) -> Right ([], rest)
           _ -> go [] s
      where
        go acc s = do
          (v, rest) <- structured lim (depth + 1) s
          let rest' = Text.stripStart rest
          case Text.uncons rest' of
            Just (',', more) ->
              let more' = Text.stripStart more
              in case Text.uncons more' of
                   Just (']', fin) -> Right (reverse (v : acc), fin)
                   _ -> go (v : acc) more'
            Just (']', fin) -> Right (reverse (v : acc), fin)
            _ -> Left NoParse

    pairs s0 =
      let s = Text.stripStart s0
      in case Text.uncons s of
           Just ('}', rest) -> Right ([], rest)
           _ -> go [] s
      where
        go acc s = do
          (k, rest) <- key s
          rest1 <- case Text.uncons (Text.stripStart rest) of
            Just (c, r) | c == '=' || c == ':' -> Right r
            _ -> Left NoParse
          (v, rest2) <- structured lim (depth + 1) rest1
          let rest3 = Text.stripStart rest2
          case Text.uncons rest3 of
            Just (',', more) ->
              let more' = Text.stripStart more
              in case Text.uncons more' of
                   Just ('}', fin) -> Right (reverse ((k, v) : acc), fin)
                   _ -> go ((k, v) : acc) more'
            Just ('}', fin) -> Right (reverse ((k, v) : acc), fin)
            _ -> Left NoParse

    key s = case Text.uncons (Text.stripStart s) of
      Just ('"', rest) -> quotedDouble "" rest
      Just ('\'', rest) -> quotedSingle rest
      _ ->
        let (k, rest) = Text.span (\c -> not (isSpace c) && c `notElem` ("=:,{}[]" :: String)) (Text.stripStart s)
        in if Text.null k then Left NoParse else Right (k, rest)

    first' f r = fmap (\(a, rest) -> (f a, rest)) r

quotedDouble :: Text -> Text -> P Text
quotedDouble acc s = case Text.uncons s of
  Nothing -> Left NoParse
  Just ('"', rest) -> Right (acc, rest)
  Just ('\\', rest) -> case Text.uncons rest of
    Just (c, rest') -> case c of
      'n' -> quotedDouble (Text.snoc acc '\n') rest'
      't' -> quotedDouble (Text.snoc acc '\t') rest'
      'r' -> quotedDouble (Text.snoc acc '\r') rest'
      _ -> quotedDouble (Text.snoc acc c) rest'
    Nothing -> Left NoParse
  Just (c, rest) -> quotedDouble (Text.snoc acc c) rest

quotedSingle :: Text -> P Text
quotedSingle s =
  let (body, rest) = Text.break (== '\'') s
  in case Text.uncons rest of
       Just ('\'', rest') -> Right (body, rest')
       _ -> Left NoParse

-- | Exact number syntax: @[+-]?digits(.digits)?([eE][+-]?digits)?@, or
-- @inf@ / @+inf@ / @-inf@ / @nan@.
parseNumber :: Text -> Maybe Number
parseNumber t = case t of
  "inf" -> Just PosInfinity
  "+inf" -> Just PosInfinity
  "-inf" -> Just NegInfinity
  "nan" -> Just NaN
  "+nan" -> Just NaN
  "-nan" -> Just NaN
  _ -> do
    let (neg, body) = case Text.uncons t of
          Just ('-', r) -> (True, r)
          Just ('+', r) -> (False, r)
          _ -> (False, t)
        (intPart, r1) = Text.span isDigit body
    if Text.null intPart then Nothing else pure ()
    (fracPart, r2) <- case Text.uncons r1 of
      Just ('.', r) ->
        let (f, r') = Text.span isDigit r
        in if Text.null f then Nothing else Just (f, r')
      _ -> Just ("", r1)
    expo <- case Text.uncons r2 of
      Nothing -> Just 0
      Just (e, r) | e == 'e' || e == 'E' ->
        let (sgn, ds) = case Text.uncons r of
              Just ('-', d) -> (-1, d)
              Just ('+', d) -> (1, d)
              _ -> (1, r)
        in if Text.null ds || not (Text.all isDigit ds) || Text.length ds > 9
             then Nothing
             else Just (sgn * read (Text.unpack ds) :: Int)
      _ -> Nothing
    let coeff = read (Text.unpack (intPart <> fracPart)) :: Integer
        signed = if neg then negate coeff else coeff
    pure (Finite (scientific signed (expo - Text.length fracPart)))

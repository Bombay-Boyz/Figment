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

import Data.Char (isDigit, isHexDigit, isSpace)
import qualified Data.Map.Strict as Map
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Figment.Error
import Figment.Internal.Number (integerFromDigits)
import Figment.Limits
import Figment.Value

-- | Parse one untyped text value. Fails only when structured text is
-- nested beyond the depth limit; any syntax problem falls back to the
-- string.
parseValue :: Limits -> Text -> Either ConfigError Value
parseValue lim raw
  | Text.compareLength raw (maxBytes lim) == GT = Left (configError (TooManyBytes (maxBytes lim)))
  | otherwise = case top of
      Left (Limit e) -> Left e
      Left NoParse -> Right (rawTagged (String raw))
      Right n
        | countNodes n > maxNodes lim -> Left (configError (TooManyNodes (maxNodes lim)))
        | otherwise -> Right (rawTagged n)
  where
    trimmed = Text.strip raw
    rawTagged n = Value untagged {tagRaw = Just raw} n
    top = case Text.uncons trimmed of
      Nothing -> Right (String raw)
      Just (c, _)
        | c == '[' || c == '{' || c == '"' || c == '\'' ->
            case structured lim 1 trimmed of
              Right (v, rest) | Text.null (Text.strip rest) -> Right (valueNode v)
              Right _ -> Left NoParse
              Left e -> Left e
        | otherwise -> Right (scalarNode raw trimmed)

-- | The number of values in a node (itself included).
countNodes :: Node -> Int
countNodes = \case
  Array xs -> 1 + sum (Vector.map (countNodes . valueNode) xs)
  Dict m -> 1 + sum (Map.map (countNodes . valueNode) m)
  _ -> 1

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
    d <- uniqueKeys kvs
    pure (mkValue (Dict d), rest')
  Just ('"', rest) -> first' string (quotedDouble [] rest)
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
      Just ('"', rest) -> quotedDouble [] rest
      Just ('\'', rest) -> quotedSingle rest
      _ ->
        let (k, rest) = Text.span (\c -> not (isSpace c) && c `notElem` ("=:,{}[]" :: String)) (Text.stripStart s)
        in if Text.null k then Left NoParse else Right (k, rest)

    -- a duplicate key is reported, as in every file format
    uniqueKeys = go Map.empty
      where
        go m [] = Right m
        go m ((k, v) : more)
          | Map.member k m = Left (Limit (configError (DuplicateKey k)))
          | otherwise = go (Map.insert k v m) more

    first' f r = fmap (\(a, rest) -> (f a, rest)) r

-- | The body of a double-quoted string, after the opening quote. Runs
-- without escapes are sliced, not appended character by character: the
-- accumulator is a list of chunks, so the whole parse is linear in the
-- length of the input.
quotedDouble :: [Text] -> Text -> P Text
quotedDouble acc s =
  let (chunk, rest) = Text.break (\c -> c == '"' || c == '\\') s
      -- continue after an escape, having produced the text @e@
      next e more = quotedDouble (e : chunk : acc) more
      unicode n r =
        let (hex, more) = Text.splitAt n r
            code = integerFromDigits 16 hex
            ok = Text.length hex == n && Text.all isHexDigit hex
         in if ok && code <= 0x10FFFF && not (code >= 0xD800 && code <= 0xDFFF)
              then next (Text.singleton (toEnum (fromInteger code))) more
              else Left NoParse
   in case Text.uncons rest of
        Just ('"', rest') -> Right (Text.concat (reverse (chunk : acc)), rest')
        Just (_, rest') -> case Text.uncons rest' of -- a backslash
          Just ('u', r) -> unicode 4 r
          Just ('U', r) -> unicode 8 r
          Just (c, rest'') -> case simpleEscape c of
            Just e -> next e rest''
            Nothing -> Left NoParse -- unknown escape: not a valid string
          Nothing -> Left NoParse
        Nothing -> Left NoParse
  where
    simpleEscape = \case
      'n' -> Just "\n"
      't' -> Just "\t"
      'r' -> Just "\r"
      'b' -> Just "\b"
      'f' -> Just "\f"
      '"' -> Just "\""
      '\\' -> Just "\\"
      _ -> Nothing

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
             else Just (sgn * fromInteger (integerFromDigits 10 ds) :: Int)
      _ -> Nothing
    let coeff = integerFromDigits 10 (intPart <> fracPart)
        signed = if neg then negate coeff else coeff
    pure (Finite (scientific signed (expo - Text.length fracPart)))

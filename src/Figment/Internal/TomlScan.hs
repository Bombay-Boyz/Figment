-- | A linear pre-scan that rejects absurdly nested TOML before the
-- (non-streaming) TOML parser is asked to build it.
--
-- toml-parser parses a whole document into a syntax tree before figment
-- sees a single value, so a megabyte of @[[[[...@ would be fully built
-- before the depth limit could apply. This scan counts unclosed @[@ and
-- @{@ outside strings and comments in O(n) time and O(1) space.
module Figment.Internal.TomlScan
  ( bracketOverflow
  , Overflow (..)
  , scanOverflow
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Figment.Metadata (Pos (..))

data Mode
  = Normal
  | Comment
  | Basic -- ^ @"…"@
  | Literal -- ^ @'…'@
  | MultiBasic -- ^ @"""…"""@
  | MultiLiteral -- ^ @'''…'''@

-- | @bracketOverflow limit text@ is the position of the first @[@ or @{@
-- that raises the bracket nesting above @limit@, if any. Brackets inside
-- strings and comments do not count. Table headers (@[a]@, @[[a]]@) count
-- like any other bracket while they are open, so callers should pass the
-- depth limit plus two to avoid rejecting valid documents.
bracketOverflow :: Int -> Text -> Maybe Pos
bracketOverflow limit txt = snd <$> scanOverflow limit maxBound txt

-- | Why a document was rejected before parsing.
data Overflow = TooDeep | TooBig
  deriving stock (Eq, Show)

-- | @scanOverflow depthLimit tokenLimit text@: like 'bracketOverflow', and
-- also rejects a document with more than @tokenLimit@ structural characters
-- (@=@, @,@, @[@, @{@, @.@) outside strings and comments. Every value in a
-- TOML document needs at least one such character per five values it
-- creates, so a limit of @5 * maxNodes@ never rejects a document that the
-- node budget would accept, but stops a huge document before toml-parser
-- builds its whole syntax tree.
scanOverflow :: Int -> Int -> Text -> Maybe (Overflow, Pos)
scanOverflow limit tokLimit txt = go Normal 0 0 0 txt
  where
    go :: Mode -> Int -> Int -> Int -> Text -> Maybe (Overflow, Pos)
    go !mode !depth !toks !off t = case Text.uncons t of
      Nothing -> Nothing
      Just (c, rest) -> case mode of
        Normal -> case c of
          '#' -> go Comment depth toks (off + 1) rest
          '"'
            | "\"\"" `Text.isPrefixOf` rest -> go MultiBasic depth toks (off + 3) (Text.drop 2 rest)
            | otherwise -> go Basic depth toks (off + 1) rest
          '\''
            | "''" `Text.isPrefixOf` rest -> go MultiLiteral depth toks (off + 3) (Text.drop 2 rest)
            | otherwise -> go Literal depth toks (off + 1) rest
          _
            | c == '[' || c == '{' ->
                if depth + 1 > limit
                  then Just (TooDeep, posAt off)
                  else tok (depth + 1)
            | c == ']' || c == '}' -> go Normal (max 0 (depth - 1)) toks (off + 1) rest
            | c == '=' || c == ',' || c == '.' -> tok depth
            | otherwise -> go Normal depth toks (off + 1) rest
            where
              tok d'
                | toks + 1 > tokLimit = Just (TooBig, posAt off)
                | otherwise = go Normal d' (toks + 1) (off + 1) rest
        Comment
          | c == '\n' -> go Normal depth toks (off + 1) rest
          | otherwise -> go Comment depth toks (off + 1) rest
        Basic -> case c of
          '\\' -> go Basic depth toks (off + 1 + skipped rest) (Text.drop 1 rest)
          '"' -> go Normal depth toks (off + 1) rest
          '\n' -> go Normal depth toks (off + 1) rest -- unterminated: the real parser reports it
          _ -> go Basic depth toks (off + 1) rest
        Literal -> case c of
          '\'' -> go Normal depth toks (off + 1) rest
          '\n' -> go Normal depth toks (off + 1) rest
          _ -> go Literal depth toks (off + 1) rest
        MultiBasic -> case c of
          '\\' -> go MultiBasic depth toks (off + 1 + skipped rest) (Text.drop 1 rest)
          '"'
            | "\"\"" `Text.isPrefixOf` rest ->
                -- the closing delimiter may be preceded by up to two content quotes
                let after = Text.drop 2 rest
                    k = Text.length (Text.takeWhile (== '"') (Text.take 2 after))
                 in go Normal depth toks (off + 3 + k) (Text.drop k after)
          _ -> go MultiBasic depth toks (off + 1) rest
        MultiLiteral -> case c of
          '\''
            | "''" `Text.isPrefixOf` rest ->
                let after = Text.drop 2 rest
                    k = Text.length (Text.takeWhile (== '\'') (Text.take 2 after))
                 in go Normal depth toks (off + 3 + k) (Text.drop k after)
          _ -> go MultiLiteral depth toks (off + 1) rest

    -- an escape consumes the next character too (if there is one)
    skipped r = if Text.null r then 0 else 1

    posAt off =
      let before = Text.take off txt
          line = 1 + Text.count "\n" before
          col = 1 + Text.length (snd (Text.breakOnEnd "\n" before))
       in Pos line col

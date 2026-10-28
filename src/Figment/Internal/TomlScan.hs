-- | A linear pre-scan that rejects absurdly nested TOML before the
-- (non-streaming) TOML parser is asked to build it.
--
-- toml-parser parses a whole document into a syntax tree before figment
-- sees a single value, so a megabyte of @[[[[...@ would be fully built
-- before the depth limit could apply. This scan counts unclosed @[@ and
-- @{@ outside strings and comments in O(n) time and O(1) space.
module Figment.Internal.TomlScan
  ( bracketOverflow
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
bracketOverflow limit txt = go Normal 0 0 txt
  where
    go :: Mode -> Int -> Int -> Text -> Maybe Pos
    go !mode !depth !off t = case Text.uncons t of
      Nothing -> Nothing
      Just (c, rest) -> case mode of
        Normal -> case c of
          '#' -> go Comment depth (off + 1) rest
          '"'
            | "\"\"" `Text.isPrefixOf` rest -> go MultiBasic depth (off + 3) (Text.drop 2 rest)
            | otherwise -> go Basic depth (off + 1) rest
          '\''
            | "''" `Text.isPrefixOf` rest -> go MultiLiteral depth (off + 3) (Text.drop 2 rest)
            | otherwise -> go Literal depth (off + 1) rest
          _
            | c == '[' || c == '{' ->
                if depth + 1 > limit
                  then Just (posAt off)
                  else go Normal (depth + 1) (off + 1) rest
            | c == ']' || c == '}' -> go Normal (max 0 (depth - 1)) (off + 1) rest
            | otherwise -> go Normal depth (off + 1) rest
        Comment
          | c == '\n' -> go Normal depth (off + 1) rest
          | otherwise -> go Comment depth (off + 1) rest
        Basic -> case c of
          '\\' -> go Basic depth (off + 1 + skipped rest) (Text.drop 1 rest)
          '"' -> go Normal depth (off + 1) rest
          '\n' -> go Normal depth (off + 1) rest -- unterminated: the real parser reports it
          _ -> go Basic depth (off + 1) rest
        Literal -> case c of
          '\'' -> go Normal depth (off + 1) rest
          '\n' -> go Normal depth (off + 1) rest
          _ -> go Literal depth (off + 1) rest
        MultiBasic -> case c of
          '\\' -> go MultiBasic depth (off + 1 + skipped rest) (Text.drop 1 rest)
          '"'
            | "\"\"" `Text.isPrefixOf` rest ->
                -- the closing delimiter may be preceded by up to two content quotes
                let after = Text.drop 2 rest
                    k = Text.length (Text.takeWhile (== '"') (Text.take 2 after))
                 in go Normal depth (off + 3 + k) (Text.drop k after)
          _ -> go MultiBasic depth (off + 1) rest
        MultiLiteral -> case c of
          '\''
            | "''" `Text.isPrefixOf` rest ->
                let after = Text.drop 2 rest
                    k = Text.length (Text.takeWhile (== '\'') (Text.take 2 after))
                 in go Normal depth (off + 3 + k) (Text.drop k after)
          _ -> go MultiLiteral depth (off + 1) rest

    -- an escape consumes the next character too (if there is one)
    skipped r = if Text.null r then 0 else 1

    posAt off =
      let before = Text.take off txt
          line = 1 + Text.count "\n" before
          col = 1 + Text.length (snd (Text.breakOnEnd "\n" before))
       in Pos line col

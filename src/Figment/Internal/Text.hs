-- | Text helpers shared by the error renderer, key paths and the
-- environment/command-line providers.
--
-- Everything figment prints about a configuration source (keys, file
-- names, variant names) is data that may have been written by someone
-- else. 'escapeControl' makes sure such data can never forge extra lines
-- or terminal escape sequences in a rendered report.
module Figment.Internal.Text
  ( needsEscape
  , escapeChar
  , escapeControl
  , isUndecodable
  ) where

import Data.Char (isControl, ord)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showHex)

-- | Characters that must never appear raw in a rendered message: control
-- characters (including newline, escape, DEL and the C1 range), the Unicode
-- line and paragraph separators, and the bidirectional overrides and
-- isolates used for "trojan source" style spoofing.
needsEscape :: Char -> Bool
needsEscape c =
  isControl c
    || c == '\x2028'
    || c == '\x2029'
    || (c >= '\x202A' && c <= '\x202E')
    || (c >= '\x2066' && c <= '\x2069')

-- | The escaped spelling of one character: @\\n@, @\\r@, @\\t@, otherwise
-- @\\u{hex}@. Total on every 'Char'.
escapeChar :: Char -> Text
escapeChar = \case
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c -> "\\u{" <> Text.pack (showHex (ord c) "") <> "}"

-- | Replace every 'needsEscape' character by its escaped spelling.
-- Returns the argument unchanged (no copy) when nothing needs escaping.
escapeControl :: Text -> Text
escapeControl t
  | Text.any needsEscape t = Text.concatMap (\c -> if needsEscape c then escapeChar c else Text.singleton c) t
  | otherwise = t

-- | GHC decodes bytes that are not valid in the locale encoding (for
-- environment variables and arguments) into the range U+DC80..U+DCFF.
-- 'Data.Text.pack' would silently turn those into U+FFFD, so callers use
-- this predicate to reject such input instead of corrupting it.
isUndecodable :: Char -> Bool
isUndecodable c = c >= '\xDC80' && c <= '\xDCFF'

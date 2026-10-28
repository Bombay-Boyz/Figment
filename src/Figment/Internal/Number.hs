-- | Fast conversion of long digit strings to 'Integer'.
--
-- @read@ and @Numeric.readHex@ fold digit by digit, which is quadratic
-- in the number of digits; a configuration source can legitimately be
-- tens of megabytes of digits in a single scalar. Splitting the text in
-- halves lets GMP's sub-quadratic multiplication do the work.
module Figment.Internal.Number
  ( integerFromDigits
  ) where

import Data.Char (digitToInt)
import Data.Text (Text)
import qualified Data.Text as Text

-- | @integerFromDigits base digits@ for @base@ 8, 10 or 16. The caller
-- guarantees every character is a digit of the base (as checked with
-- 'Data.Char.isDigit', 'Data.Char.isOctDigit' or 'Data.Char.isHexDigit').
-- The empty text is 0.
integerFromDigits :: Int -> Text -> Integer
integerFromDigits base = go
  where
    b = toInteger base
    -- digits that always fit in an Int (< 2^62)
    chunk = case base of
      16 -> 15
      8 -> 20
      _ -> 18
    small = toInteger . Text.foldl' (\acc c -> acc * base + digitToInt c) (0 :: Int)
    go t
      | Text.compareLength t chunk /= GT = small t
      | otherwise =
          let n = Text.length t
              h = n `div` 2
              (hi, lo) = Text.splitAt h t
           in go hi * b ^ (n - h) + go lo

-- | TOML (v1.0, via toml-parser). Strict UTF-8; every value carries
-- its line and column; dates and times become ISO 8601 strings (decode
-- them into "Data.Time" types directly); @inf@ and @nan@ are kept as
-- non-finite numbers rather than corrupted.
module Figment.Provider.Toml
  ( toml
  , parseToml
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Toml.Semantics as T
import qualified Toml.Syntax as T

import Figment.Error
import Figment.Internal.Budget
import Figment.Internal.TomlScan (bracketOverflow)
import Figment.Limits
import Figment.Metadata (Pos (..))
import Figment.Path
import Figment.Provider.Format (Format (..))
import Figment.Value

-- | The TOML format.
toml :: Format
toml = Format "TOML" (\lim bs -> pure (parseToml lim bs))

-- | Parse a TOML document, enforcing the limits.
parseToml :: Limits -> ByteString -> Either ConfigError Value
parseToml lim bs = do
  checkBytes lim bs
  txt <- case TE.decodeUtf8' bs of
    Left e -> Left (configError (SyntaxError ("invalid UTF-8: " <> Text.pack (show e))))
    Right t -> Right t
  -- toml-parser builds its whole syntax tree before figment sees a single
  -- value, so a pathologically nested document is rejected here, by one
  -- linear pass, before the parser is asked to build it. Table headers
  -- count as brackets while open, hence the allowance of two.
  case bracketOverflow (if maxDepth lim >= maxBound - 2 then maxBound else maxDepth lim + 2) txt of
    Just at -> Left (withPos (Just at) (configError (DepthExceeded (maxDepth lim))))
    Nothing -> Right ()
  exprs <- case T.parseRawToml txt of
    Left (T.Located p msg) -> Left (withPos (Just (pos p)) (configError (SyntaxError (Text.pack msg))))
    Right es -> Right es
  tab <- case T.semantics exprs of
    Left (T.SemanticError ann key kind) ->
      Left (withPos (Just (pos ann)) (configError (semantic key kind)))
    Right t -> Right t
  runBudget lim (table 1 root Nothing tab)
  where
    semantic key = \case
      T.AlreadyAssigned -> DuplicateKey key
      T.ClosedTable -> SyntaxError ("table `" <> key <> "` was already closed")
      T.ImplicitlyTable -> SyntaxError ("`" <> key <> "` was implicitly defined as a table")

pos :: T.Position -> Pos
pos p = Pos (T.posLine p) (T.posColumn p)

table :: Int -> Path -> Maybe Pos -> T.Table' T.Position -> Budget Value
table d p at (T.MkTable m) = do
  spend 1 p at
  enterContainer d p at
  kvs <- Map.traverseWithKey (\k (_, v) -> value (d + 1) (p /. k) v) m
  pure (Value untagged {tagPos = at} (Dict kvs))

value :: Int -> Path -> T.Value' T.Position -> Budget Value
value d p v = case v of
  T.Table' a t -> table d p (Just (pos a)) t
  T.List' a xs -> do
    spend 1 p (Just (pos a))
    enterContainer d p (Just (pos a))
    ys <- traverse (\(i, x) -> value (d + 1) (p /# i) x) (zip [0 ..] xs)
    pure (Value (at a) (Array (Vector.fromList ys)))
  T.Integer' a n -> leaf a (Num (Finite (fromInteger n)))
  T.Double' a x -> leaf a (Num (double x))
  T.Bool' a b -> leaf a (Bool b)
  T.Text' a t -> leaf a (String t)
  T.TimeOfDay' a t -> leaf a (String (Text.pack (iso8601Show t)))
  T.ZonedTime' a t -> leaf a (String (Text.pack (iso8601Show t)))
  T.LocalTime' a t -> leaf a (String (Text.pack (iso8601Show t)))
  T.Day' a t -> leaf a (String (Text.pack (iso8601Show t)))
  where
    at a = untagged {tagPos = Just (pos a)}
    leaf a n = do
      spend 1 p (Just (pos a))
      pure (Value (at a) n)
    double x
      | isNaN x = NaN
      | isInfinite x = if x > 0 then PosInfinity else NegInfinity
      | otherwise = Finite (Sci.fromFloatDigits x)

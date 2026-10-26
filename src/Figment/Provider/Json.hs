-- | JSON. Parsed from aeson's lazy token stream, so the depth and size
-- limits are enforced while parsing (a megabyte of @[[[[...@ is
-- rejected at depth 65 without being materialised) and duplicate keys
-- are reported instead of silently keeping the last.
module Figment.Provider.Json
  ( json
  , parseJson
  ) where

import qualified Data.Aeson.Decoding.ByteString as Tok
import Data.Aeson.Decoding.Tokens
import qualified Data.Aeson.Key as Key
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Figment.Error
import Figment.Internal.Budget
import Figment.Limits
import Figment.Path
import Figment.Provider.Format (Format (..))
import Figment.Value

-- | The JSON format.
json :: Format
json = Format "JSON" (\lim bs -> pure (parseJson lim bs))

-- | Parse a JSON document, enforcing the limits.
parseJson :: Limits -> ByteString -> Either ConfigError Value
parseJson lim bs = runBudget lim $ do
  (v, rest) <- value 1 root (Tok.bsToTokens bs)
  if BC.all (`elem` (" \t\r\n" :: String)) rest
    then pure v
    else throwB (configError (SyntaxError "unexpected characters after the JSON value"))

syntax :: Path -> String -> Budget a
syntax p e = throwB (atPath p (configError (SyntaxError (Text.pack e))))

value :: forall k. Int -> Path -> Tokens k String -> Budget (Value, k)
value d p tok = do
  spend 1 p Nothing
  case tok of
    TkLit LitNull k -> pure (mkValue Null, k)
    TkLit LitTrue k -> pure (mkValue (Bool True), k)
    TkLit LitFalse k -> pure (mkValue (Bool False), k)
    TkText t k -> pure (mkValue (String t), k)
    TkNumber n k -> pure (mkValue (Num (Finite (num n))), k)
    TkArrayOpen arr -> enterContainer d p Nothing >> items 0 [] arr
    TkRecordOpen rec -> enterContainer d p Nothing >> pairs Map.empty rec
    TkErr e -> syntax p e
  where
    num (NumInteger i) = fromInteger i
    num (NumDecimal s) = s
    num (NumScientific s) = s

    items :: Int -> [Value] -> TkArray k String -> Budget (Value, k)
    items i acc = \case
      TkItem toks -> do
        (v, rest) <- value (d + 1) (p /# i) toks
        items (i + 1) (v : acc) rest
      TkArrayEnd k -> pure (mkValue (Array (Vector.fromList (reverse acc))), k)
      TkArrayErr e -> syntax p e

    pairs :: Map.Map Text.Text Value -> TkRecord k String -> Budget (Value, k)
    pairs acc = \case
      TkPair key toks -> do
        let kt = Key.toText key
        if Map.member kt acc
          then throwB (atPath (p /. kt) (configError (DuplicateKey kt)))
          else do
            (v, rest) <- value (d + 1) (p /. kt) toks
            pairs (Map.insert kt v acc) rest
      TkRecordEnd k -> pure (mkValue (Dict acc), k)
      TkRecordErr e -> syntax p e

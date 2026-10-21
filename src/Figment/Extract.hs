{-# LANGUAGE Trustworthy #-}
-- | @runExtract@ — decode a merged 'Tagged' tree into the caller's
-- record type via aeson's 'FromJSON' (feasibility study §8.2,
-- ADR 006).
module Figment.Extract (runExtract) where

import Data.Aeson (FromJSON, Result(..), fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Data.Text as Text

import Figment.Error (ConfigError(..))
import Figment.Value
  ( Tagged(..), Value(..), Metadata(..), Key(..), Path(..) )

runExtract :: FromJSON a => Tagged -> Either ConfigError a
runExtract t =
  case fromJSON (toAeson (tVal t)) of
    Success a -> Right a
    Error msg ->
      Left (DecodeError (firstLeafPath t) (Text.pack msg))

toAeson :: Value -> Aeson.Value
toAeson VNull      = Aeson.Null
toAeson (VBool b)  = Aeson.Bool b
toAeson (VNum n)   = Aeson.Number n
toAeson (VStr t)   = Aeson.String t
toAeson (VArr xs)  = Aeson.Array (Vector.fromList (map (toAeson . tVal) xs))
toAeson (VObj m)   =
  Aeson.Object (Aeson.KeyMap.fromList
    [ (Aeson.Key.fromText k, toAeson (tVal v)) | (Key k, v) <- Map.toList m ])

firstLeafPath :: Tagged -> Path
firstLeafPath (Tagged v _) = go v
  where
    go VNull         = Path []
    go (VBool _)     = Path []
    go (VNum _)      = Path []
    go (VStr _)      = Path []
    go (VObj m)      = case Map.toList m of
                         ((_, sub):_) -> childPath sub
                         []           -> Path []
    go (VArr (x:_))  = childPath x
    go (VArr [])     = Path []

    childPath (Tagged _ (Leaf _ _ p)) = p
    childPath (Tagged inner _)        = go inner

-- | Turning a loaded configuration into typed values.
module Figment.Extract
  ( extract
  , extractLossy
  , extractAt
  , extractWith
  , extractPartial
  , extractPartialWith
  , extractTotal
  , extractTotalWith
  , loadExtract
  , loadExtractOrThrow
  , decodeContext
  ) where

import Control.Exception (throwIO)
import Data.Functor.Identity (Identity)
import Data.Semigroup (sconcat)

import Figment.Core
import Figment.Decode
import Figment.Error
import Figment.Partial
import Figment.Path
import Figment.Value

-- | The context decoders run in for a loaded configuration.
decodeContext :: Loaded -> DecodeContext
decodeContext l = rootContext (loadedProfile l) (loadedSources l)

-- | Decode the selected profile.
extract :: FromConfig a => Loaded -> Either ConfigErrors a
extract = extractWith fromConfig

-- | Like 'extract', but accept @"8080"@ for numbers, @"yes"@ for
-- booleans, and scalars for strings (Rust: @extract_lossy@).
extractLossy :: FromConfig a => Loaded -> Either ConfigErrors a
extractLossy l = runDecoder fromConfig (decodeContext l) {ctxLossy = True} (selected l)

-- | Decode the selected profile with an explicit decoder.
extractWith :: Decoder a -> Loaded -> Either ConfigErrors a
extractWith d l = runDecoder d (decodeContext l) (selected l)

-- | Decode the value at a key path (Rust: @extract_inner@).
extractAt :: forall a. FromConfig a => Path -> Loaded -> Either ConfigErrors a
extractAt p l = case lookupPath p (selected l) of
  Just v -> runDecoder fromConfig (decodeContext l) {ctxPath = p} v
  Nothing -> case missingValue @a of
    Just d -> Right d
    Nothing -> Left (single (missingAt (decodeContext l) p))

-- | Decode a 'Partial' configuration, filling gaps from partial
-- defaults (provider values win). Reports every type error, or else
-- every missing required field.
extractPartial :: Partial c => c Maybe -> Loaded -> Either ConfigErrors (c Identity)
extractPartial = extractPartialWith defaultOptions

-- | 'extractPartial' with explicit generic t'Options' (e.g. 'snakeCase' field names).
extractPartialWith :: Partial c => Options -> c Maybe -> Loaded -> Either ConfigErrors (c Identity)
extractPartialWith o defs l = do
  found <- runDecoder (partialDecoder o) (decodeContext l) (selected l)
  case complete o (overlay defs found) of
    Right r -> Right r
    Left ps ->
      let errs = fmap (missingAt (decodeContext l)) ps
      in Left (sconcat (fmap single errs))

-- | With /total/ defaults a missing-field error is impossible by
-- construction: the type guarantees every field has a value.
extractTotal :: Partial c => c Identity -> Loaded -> Either ConfigErrors (c Identity)
extractTotal = extractTotalWith defaultOptions

-- | 'extractTotal' with explicit generic t'Options'.
extractTotalWith :: Partial c => Options -> c Identity -> Loaded -> Either ConfigErrors (c Identity)
extractTotalWith o d = extractPartialWith o (total d)

missingAt :: DecodeContext -> Path -> ConfigError
missingAt c p = case reverse (segments p) of
  (Key name : parentRev) -> missingFieldError c {ctxPath = fromSegments (reverse parentRev)} name
  _ -> atPath p (configError (MissingField (renderPath p)))

-- | 'load' then 'extract'.
loadExtract :: FromConfig a => Figment -> IO (Either ConfigErrors a)
loadExtract fig = (>>= extract) <$> load fig

-- | 'loadExtract', throwing t'ConfigErrors' (whose 'Control.Exception.displayException' is
-- the rendered report).
loadExtractOrThrow :: FromConfig a => Figment -> IO a
loadExtractOrThrow fig = loadExtract fig >>= either throwIO pure

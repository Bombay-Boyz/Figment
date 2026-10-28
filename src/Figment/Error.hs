-- | The error vocabulary. Every failure anywhere in figment is a
-- t'ConfigError'; operations that can fail in more than one place return
-- t'ConfigErrors', a non-empty collection, so users see /every/ problem
-- in one run instead of fixing them one at a time.
module Figment.Error
  ( ErrorKind (..)
  , ConfigError (..)
  , ConfigErrors
  , configError
  , single
  , errorList
  , fromErrorList
  , atPath
  , withMetadata
  , withPos
  , withProfile
  , addHints
  , describeKind
  , renderError
  , renderErrors
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Figment.Internal.Text (escapeControl)
import Figment.Metadata
import Figment.Path
import Figment.Profile

-- | What went wrong. Closed: every constructor is produced somewhere in
-- the library.
data ErrorKind
  = IoError !Text
    -- ^ a source could not be read
  | SyntaxError !Text
    -- ^ a source could not be parsed
  | DepthExceeded !Int
    -- ^ nesting deeper than the configured limit (the limit is carried)
  | TooManyNodes !Int
    -- ^ more values than the configured limit (the limit is carried)
  | TooManyBytes !Int
    -- ^ a source larger than the configured size limit (the limit, in
    -- bytes, is carried)
  | DuplicateKey !Text
    -- ^ the same key appears twice in one dictionary of one source
  | KeyConflict !Text !Text
    -- ^ two flat keys (env vars, CLI flags) that cannot both hold: one
    -- sets a value where the other needs a dictionary
  | InvalidType !Text !Text
    -- ^ @InvalidType actual expected@
  | InvalidValue !Text !Text
    -- ^ right type, unacceptable value: @InvalidValue actual expected@
  | InvalidLength !Int !Text
    -- ^ @InvalidLength actualLength expected@
  | MissingField !Text
    -- ^ a required field has no value in any source
  | UnknownField !Text ![Text]
    -- ^ field not expected by the target type; carries suggestions
  | UnknownVariant !Text ![Text]
    -- ^ enum tag not recognised; carries the valid tags
  | Message !Text
    -- ^ a custom failure, e.g. from a validation step
  | InvalidArgument !Text
    -- ^ the library API was called with an invalid argument
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | One error, with everything known about where it happened.
data ConfigError = ConfigError
  { errorKind :: !ErrorKind
  , errorPath :: !Path
    -- ^ key path within the selected profile
  , errorProfile :: !(Maybe Profile)
    -- ^ the profile the offending value came from
  , errorMetadata :: !(Maybe Metadata)
    -- ^ the provider the offending value came from
  , errorPos :: !(Maybe Pos)
    -- ^ line/column in that provider's source text, when known
  , errorHints :: ![Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | One or more errors. A 'Semigroup', so independent failures combine.
newtype ConfigErrors = ConfigErrors (NonEmpty ConfigError)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
  deriving newtype (Semigroup)

instance Exception ConfigErrors where
  displayException = Text.unpack . renderErrors

-- | An error of the given kind at the root, with no provenance yet.
configError :: ErrorKind -> ConfigError
configError k = ConfigError k root Nothing Nothing Nothing []

-- | One error.
single :: ConfigError -> ConfigErrors
single e = ConfigErrors (e :| [])

-- | All errors, in order.
errorList :: ConfigErrors -> [ConfigError]
errorList (ConfigErrors es) = NE.toList es

-- | Collect errors, if there are any.
fromErrorList :: [ConfigError] -> Maybe ConfigErrors
fromErrorList = fmap ConfigErrors . NE.nonEmpty

-- | Set the path.
atPath :: Path -> ConfigError -> ConfigError
atPath p e = e {errorPath = p}

-- | Set the provider metadata.
withMetadata :: Maybe Metadata -> ConfigError -> ConfigError
withMetadata m e = e {errorMetadata = m}

-- | Set the source position.
withPos :: Maybe Pos -> ConfigError -> ConfigError
withPos p e = e {errorPos = p}

-- | Set the profile.
withProfile :: Maybe Profile -> ConfigError -> ConfigError
withProfile p e = e {errorProfile = p}

-- | Append hints.
addHints :: [Text] -> ConfigError -> ConfigError
addHints hs e = e {errorHints = errorHints e ++ hs}

-- | The one-line description of an error kind.
describeKind :: ErrorKind -> Text
describeKind = \case
  IoError t -> "could not read source: " <> escapeControl t
  SyntaxError t -> "syntax error: " <> escapeControl t
  DepthExceeded n -> "nesting exceeds the depth limit of " <> tshow n
  TooManyNodes n -> "source contains more than the limit of " <> tshow n <> " values"
  TooManyBytes n -> "source is larger than the limit of " <> tshow n <> " bytes"
  DuplicateKey k -> "duplicate key " <> tick k
  KeyConflict a b -> tick a <> " conflicts with " <> tick b
    <> ": one sets a value where the other needs a dictionary"
  InvalidType actual expected -> "invalid type: found " <> actual <> ", expected " <> expected
  InvalidValue actual expected -> "invalid value: found " <> actual <> ", expected " <> expected
  InvalidLength n expected -> "invalid length " <> tshow n <> ", expected " <> expected
  MissingField f -> "missing field " <> tick f
  UnknownField f _ -> "unknown field " <> tick f
  UnknownVariant v valid -> "unknown variant " <> tick v <> ", expected one of "
    <> Text.intercalate ", " (map tick valid)
  Message t -> t
  InvalidArgument t -> "invalid argument: " <> t

-- | A multi-line, human-oriented rendering:
--
-- > error: invalid type: found string "abc", expected an integer
-- >   --> key `port` (`APP_PORT`) in environment variables
-- >   = hint: ...
renderError :: ConfigError -> Text
renderError e =
  Text.intercalate "\n" $
    ("error: " <> describeKind (errorKind e))
      : location
      ++ map ("  = hint: " <>) (suggestions ++ errorHints e)
  where
    suggestions = case errorKind e of
      UnknownField _ (s : ss) -> ["did you mean " <> Text.intercalate " or " (map tick (s : ss)) <> "?"]
      _ -> []
    prof = maybe Default id (errorProfile e)
    path = errorPath e
    keyPart
      | isRoot path = "top level"
      | otherwise = "key " <> tick (renderPath path)
    native = case errorMetadata e of
      Just m | not (isRoot path) ->
        let n = interpolate m prof path
        in if n == renderPath path then "" else " (" <> tick n <> ")"
      _ -> ""
    sourcePart = case errorMetadata e of
      Nothing -> ""
      Just m -> " in " <> escapeControl (metaName m) <> case metaSource m of
        Just s -> " " <> escapeControl (renderSource s) <> posSuffix
        Nothing -> posWords
    posSuffix = case errorPos e of
      Just (Pos l c) -> ":" <> tshow l <> ":" <> tshow c
      Nothing -> ""
    posWords = case errorPos e of
      Just (Pos l c) -> " at line " <> tshow l <> ", column " <> tshow c
      Nothing -> ""
    profPart = case errorProfile e of
      Just p | p /= Default -> " [profile " <> tick (profileName p) <> "]"
      _ -> ""
    location =
      let line = keyPart <> native <> sourcePart <> profPart
      in ["  --> " <> line]

-- | Render every error, separated by blank lines, with a count header
-- when there is more than one. At most 'renderLimit' errors are shown,
-- followed by the number that were left out, so a pathological source
-- cannot produce an unreadable (or enormous) report.
renderErrors :: ConfigErrors -> Text
renderErrors (ConfigErrors (e :| [])) = renderError e
renderErrors (ConfigErrors es) =
  tshow (length es) <> " configuration errors:\n\n"
    <> Text.intercalate "\n\n" (map renderError shown)
    <> omitted
  where
    (shown, rest) = splitAt renderLimit (NE.toList es)
    omitted
      | null rest = ""
      | otherwise = "\n\n... and " <> tshow (length rest) <> " more errors not shown"

-- | The most errors 'renderErrors' prints.
renderLimit :: Int
renderLimit = 100

-- | Quote a piece of data for a message. Control characters are escaped, so
-- a key or file name chosen by someone else cannot forge extra lines or
-- terminal escape sequences in a report.
tick :: Text -> Text
tick t = "`" <> escapeControl t <> "`"

tshow :: Show a => a -> Text
tshow = Text.pack . show

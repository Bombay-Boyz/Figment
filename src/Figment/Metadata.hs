-- | Where configuration values come from.
module Figment.Metadata
  ( Metadata (..)
  , Source (..)
  , Pos (..)
  , named
  , fileMetadata
  , withSource
  , withInterpolater
  , withAddedAt
  , interpolate
  , renderSource
  , renderSrcLoc
  ) where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Stack (SrcLoc (..))

import Figment.Path (Path, renderPath)
import Figment.Profile (Profile)

-- | A one-based line and column in a source text.
data Pos = Pos
  { posLine :: !Int
  , posColumn :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The origin of a provider's data.
data Source
  = FileSource !FilePath   -- ^ a file on disk (resolved path)
  | CodeSource !SrcLoc     -- ^ values written in Haskell code
  | CustomSource !Text     -- ^ anything else (\"environment\", a URL, ...)
  deriving stock (Eq, Show, Generic)

instance NFData Source where
  rnf (FileSource p) = rnf p
  rnf (CodeSource l) = l `seq` ()
  rnf (CustomSource t) = rnf t

-- | Everything known about a provider.
--
-- 'metaInterpolate' turns a key path back into the provider's own
-- vocabulary, e.g. @db.port@ into @APP_DB\__PORT@ for environment
-- variables or @--db.port@ for CLI arguments; error messages and
-- missing-field hints use it so users are told what to /type/.
data Metadata = Metadata
  { metaName :: !Text
  , metaSource :: !(Maybe Source)
  , metaAddedAt :: !(Maybe SrcLoc)
  , metaInterpolate :: Profile -> Path -> Text
  }
  deriving stock (Generic)

-- | Equality ignores the interpolation function (functions have no
-- equality); two metadata are equal when they name the same source
-- added at the same place.
instance Eq Metadata where
  a == b =
    metaName a == metaName b
      && metaSource a == metaSource b
      && metaAddedAt a == metaAddedAt b

instance Show Metadata where
  showsPrec d m = showParen (d > 10) $
    showString "Metadata {metaName = " . shows (metaName m)
      . showString ", metaSource = " . shows (metaSource m)
      . showString ", metaAddedAt = " . shows (metaAddedAt m)
      . showString "}"

instance NFData Metadata where
  rnf (Metadata n s a i) = rnf n `seq` rnf s `seq` maybe () (`seq` ()) a `seq` rwhnf i

-- | Metadata with a name and the default interpolater (the rendered
-- key path).
named :: Text -> Metadata
named n = Metadata n Nothing Nothing (const renderPath)

-- | Metadata for a file-backed source.
fileMetadata :: Text -> FilePath -> Metadata
fileMetadata n p = withSource (FileSource p) (named n)

-- | Set the source.
withSource :: Source -> Metadata -> Metadata
withSource s m = m {metaSource = Just s}

-- | Replace the key-name renderer.
withInterpolater :: (Profile -> Path -> Text) -> Metadata -> Metadata
withInterpolater f m = m {metaInterpolate = f}

-- | Record where the provider was added.
withAddedAt :: SrcLoc -> Metadata -> Metadata
withAddedAt l m = m {metaAddedAt = Just l}

-- | The provider-native name of a key.
interpolate :: Metadata -> Profile -> Path -> Text
interpolate = metaInterpolate

-- | A short rendering of a source.
renderSource :: Source -> Text
renderSource (FileSource p) = Text.pack p
renderSource (CodeSource l) = renderSrcLoc l
renderSource (CustomSource t) = t

-- | @file:line:column@.
renderSrcLoc :: SrcLoc -> Text
renderSrcLoc l =
  Text.pack (srcLocFile l) <> ":" <> Text.pack (show (srcLocStartLine l))
    <> ":" <> Text.pack (show (srcLocStartCol l))

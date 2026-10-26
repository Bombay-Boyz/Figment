-- | File- and string-backed providers for any data format (Rust:
-- @Format@ / @Data\<F\>@).
--
-- > F.merge (file toml "App.toml")              -- optional, searched upward
-- > F.merge (required (fileExact json "/etc/app.json"))
-- > F.merge (nested (file yaml "profiles.yaml")) -- top-level keys are profiles
module Figment.Provider.Format
  ( Format (..)
  , Data
  , file
  , fileExact
  , fromText
  , nested
  , required
  , inProfile
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath (isRelative, takeDirectory, (</>))

import Figment.Core
import Figment.Error
import Figment.Limits
import Figment.Metadata
import Figment.Path
import Figment.Profile
import Figment.Value

-- | A data format: a name and a parser. The parser must enforce the
-- 'Limits' and should record 'tagPos' on values when it can.
data Format = Format
  { formatName :: Text
  , formatParse :: Limits -> ByteString -> IO (Either ConfigError Value)
  }

data Origin
  = FromFile !FilePath !Bool -- ^ path, search parent directories?
  | FromText !Text

-- | A file or string in some t'Format'; build with 'file', 'fileExact' or 'fromText'.
data Data = Data
  { dataFormat :: Format
  , dataOrigin :: Origin
  , dataNested :: Bool
  , dataRequired :: Bool
  , dataProfile :: Profile
  }

-- | An optional file. A relative path is looked for in the working
-- directory and then each parent directory (as Rust figment does); a
-- missing file contributes nothing. Combine with 'required' to make
-- absence an error.
file :: Format -> FilePath -> Data
file f p = Data f (FromFile p True) False False Default

-- | Like 'file', but only the exact path is tried.
fileExact :: Format -> FilePath -> Data
fileExact f p = Data f (FromFile p False) False False Default

-- | Configuration text held in memory (Rust: @Format::string@).
fromText :: Format -> Text -> Data
fromText f t = Data f (FromText t) False False Default

-- | Treat top-level keys as profile names.
nested :: Data -> Data
nested d = d {dataNested = True}

-- | Fail if the file cannot be found.
required :: Data -> Data
required d = d {dataRequired = True}

-- | Put the data in a profile other than 'Default' (ignored when
-- 'nested').
inProfile :: Profile -> Data -> Data
inProfile p d = d {dataProfile = p}

baseMetadata :: Data -> Maybe FilePath -> Metadata
baseMetadata d resolved =
  let fmt = formatName (dataFormat d)
      m = case (dataOrigin d, resolved) of
        (_, Just p) -> fileMetadata (fmt <> " file") p
        (FromFile p _, Nothing) -> fileMetadata (fmt <> " file") p
        (FromText _, Nothing) -> named (fmt <> " source string")
      interp
        | dataNested d = \prof path -> profileName prof <> (if isRoot path then "" else "." <> renderPath path)
        | otherwise = const renderPath
  in withInterpolater interp m

instance Provider Data where
  metadata d = baseMetadata d Nothing
  provide lim d = case dataOrigin d of
    FromText t -> parseWith (baseMetadata d Nothing) (TE.encodeUtf8 t)
    FromFile p search -> do
      found <- locate search p
      case found of
        Nothing
          | dataRequired d ->
              pure (Left (single (withMetadata (Just (baseMetadata d Nothing))
                (configError (IoError ("file not found: " <> Text.pack p
                  <> (if search && isRelative p then " (searched the working directory and its parents)" else "")))))))
          | otherwise -> pure (Right Map.empty)
        Just resolved -> do
          let meta = baseMetadata d (Just resolved)
          r <- try (BS.readFile resolved)
          case r of
            Left (e :: IOException) ->
              pure (Left (single (withMetadata (Just meta) (configError (IoError (Text.pack (show e)))))))
            Right bytes -> parseWith meta bytes
    where
      parseWith meta bytes = do
        parsed <- formatParse (dataFormat d) lim bytes
        pure $ case parsed of
          Left e -> Left (single (withMetadata (Just meta) e))
          Right v -> case valueNode v of
            Dict top
              | dataNested d -> splitProfiles meta top
              | otherwise -> Right (Map.singleton (dataProfile d) (fmap (retag meta (dataProfile d)) top))
            n -> Left (single (withMetadata (Just meta) (withPos (tagPos (valueTag v))
                   (configError (InvalidType (describe n) "a dictionary at the top level")))))

      splitProfiles meta top =
        let one (k, v) = case valueNode v of
              Dict d' -> let p = profile k in Right (p, fmap (retag meta p) d')
              n -> Left (withMetadata (Just meta) (withPos (tagPos (valueTag v))
                     (atPath (root /. k) (configError (InvalidType (describe n) "a dictionary (nested files map profile names to dictionaries)")))))
            results = map one (Map.toList top)
        in case fromErrorList [e | Left e <- results] of
             Just es -> Left es
             Nothing -> Right (Map.fromListWith (\later earlier -> Map.union earlier later) [x | Right x <- results])

-- | Find a file, optionally searching parent directories. Returns an
-- absolute path.
locate :: Bool -> FilePath -> IO (Maybe FilePath)
locate search p
  | search && isRelative p = getCurrentDirectory >>= go
  | otherwise = do
      ok <- doesFileExist p
      if ok then Just <$> makeAbsolute p else pure Nothing
  where
    go dir = do
      let candidate = dir </> p
      ok <- doesFileExist candidate
      if ok
        then pure (Just candidate)
        else
          let parent = takeDirectory dir
          in if parent == dir then pure Nothing else go parent

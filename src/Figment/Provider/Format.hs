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
import System.Directory (doesDirectoryExist, getCurrentDirectory, getFileSize, makeAbsolute)
import System.FilePath (isRelative, takeDirectory, (</>))
import System.IO (IOMode (ReadMode), hFileSize, withBinaryFile)
import System.IO.Error (isPermissionError)

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
        Left e ->
          -- the place the file would be could not even be inspected
          pure (Left (single (withMetadata (Just (baseMetadata d Nothing)) (configError (IoError (Text.pack (show e)))))))
        Right Nothing
          | dataRequired d ->
              pure (Left (single (withMetadata (Just (baseMetadata d Nothing))
                (configError (IoError ("file not found: " <> Text.pack p
                  <> (if search && isRelative p then " (searched the working directory and its parents)" else "")))))))
          | otherwise -> pure (Right Map.empty)
        Right (Just resolved) -> do
          let meta = baseMetadata d (Just resolved)
          r <- try (readBounded (maxBytes lim) resolved)
          case r of
            Left (e :: IOException) ->
              pure (Left (single (withMetadata (Just meta) (configError (IoError (Text.pack (show e)))))))
            Right TooBig ->
              pure (Left (single (withMetadata (Just meta) (configError (TooManyBytes (maxBytes lim))))))
            Right (Contents bytes) -> parseWith meta bytes
    where
      parseWith meta bytes = do
        parsed <- formatParse (dataFormat d) lim bytes
        pure $ case parsed of
          Left e -> Left (single (withMetadata (Just meta) e))
          Right v -> case valueNode v of
            Dict top
              | dataNested d -> splitProfiles meta top
              | otherwise -> Right (Map.singleton (dataProfile d) (Map.map (retag meta (dataProfile d)) top))
            n -> Left (single (withMetadata (Just meta) (withPos (tagPos (valueTag v))
                   (configError (InvalidType (describe n) "a dictionary at the top level")))))

      splitProfiles meta top =
        let one (k, v) = case valueNode v of
              Dict d' -> let p = profile k in Right (p, Map.map (retag meta p) d')
              n -> Left (withMetadata (Just meta) (withPos (tagPos (valueTag v))
                     (atPath (root /. k) (configError (InvalidType (describe n) "a dictionary (nested files map profile names to dictionaries)")))))
            results = map one (Map.toList top)
        in case fromErrorList [e | Left e <- results] of
             Just es -> Left es
             Nothing -> Right (Map.fromListWith (\later earlier -> Map.union earlier later) [x | Right x <- results])

-- | Find a file, optionally searching parent directories. Returns an
-- absolute path.
--
-- Absence is @Right Nothing@. A failure to even look for the file (for
-- instance a permission error on the working directory) is a @Left@: an
-- unreadable location is reported, never mistaken for an absent file that
-- would silently leave the defaults in force. Only an unreadable
-- /ancestor/ of the working directory is skipped when searching upwards.
locate :: Bool -> FilePath -> IO (Either IOException (Maybe FilePath))
locate search p
  | search && isRelative p = getCurrentDirectory >>= first
  | otherwise = do
      r <- probeFile p
      case r of
        Left e -> pure (Left e)
        Right True -> Right . Just <$> makeAbsolute p
        Right False -> pure (Right Nothing)
  where
    first dir = do
      r <- probeFile (dir </> p)
      case r of
        Left e -> pure (Left e)
        Right True -> pure (Right (Just (dir </> p)))
        Right False -> up dir
    up dir =
      let parent = takeDirectory dir
       in if parent == dir
            then pure (Right Nothing)
            else do
              r <- probeFile (parent </> p)
              case r of
                Right True -> pure (Right (Just (parent </> p)))
                _ -> up parent

-- | Is there a regular file (or something that is not a directory) at the
-- path? Distinguishes "no" from "could not tell because of permissions".
probeFile :: FilePath -> IO (Either IOException Bool)
probeFile p = do
  r <- try (getFileSize p)
  case r of
    Right _ -> Right . not <$> doesDirectoryExist p
    Left e
      | isPermissionError e -> pure (Left e)
      | otherwise -> pure (Right False)

data ReadOutcome = Contents !ByteString | TooBig

-- | Read a file, giving up as soon as it is known to exceed @limit@ bytes.
-- A regular file larger than the limit is never opened for reading; any
-- other file (a pipe, a device) is read in chunks and abandoned at the
-- limit, so memory use is bounded by the limit whatever the file claims.
readBounded :: Int -> FilePath -> IO ReadOutcome
readBounded limit path = withBinaryFile path ReadMode $ \h -> do
  size <- try (hFileSize h)
  case size of
    Right (n :: Integer) | n > toInteger limit -> pure TooBig
    Right _ -> loop h 0 []
    Left (_ :: IOException) -> loop h 0 []
  where
    loop h !total acc = do
      chunk <- BS.hGetSome h 65536
      if BS.null chunk
        then pure (Contents (BS.concat (reverse acc)))
        else
          let total' = total + BS.length chunk
           in if total' > limit then pure TooBig else loop h total' (chunk : acc)

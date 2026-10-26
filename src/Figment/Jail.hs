-- | A sandbox for configuration tests (Rust: @figment::Jail@).
--
-- 'withJail' runs an action in a fresh temporary working directory with
-- a snapshot of the environment, and restores both afterwards, even on
-- exceptions. Jails are serialised process-wide (the working directory
-- and the environment are process-global), so jailed tests are safe
-- under a parallel test runner. Tests that do not use a jail should not
-- touch the working directory or the environment.
module Figment.Jail
  ( Jail
  , withJail
  , jailDirectory
  , createFile
  , createBinaryFile
  , createDirectory
  , inDirectory
  , setEnv
  , unsetEnv
  , clearEnv
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, bracket_)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import qualified System.Environment as Env
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)

-- | A handle on the running jail.
newtype Jail = Jail
  { jailDirectory :: FilePath
    -- ^ the temporary directory (also the working directory)
  }

-- The one piece of global state: a lock serialising jails. Standard
-- top-level-MVar idiom; NOINLINE guarantees a single MVar.
jailLock :: MVar ()
jailLock = unsafePerformIO (newMVar ())
{-# NOINLINE jailLock #-}

-- | Run an action in a fresh jail; see the module header.
withJail :: (Jail -> IO a) -> IO a
withJail act = withMVar jailLock $ \() ->
  withSystemTempDirectory "figment-jail" $ \dir ->
    bracket Env.getEnvironment restoreEnv $ \_ ->
      bracket getCurrentDirectory setCurrentDirectory $ \_ ->
        bracket_ (setCurrentDirectory dir) (pure ()) (act (Jail dir))
  where
    restoreEnv saved = do
      now <- Env.getEnvironment
      mapM_ (Env.unsetEnv . fst) [kv | kv@(k, _) <- now, k `notElem` map fst saved]
      mapM_ (uncurry Env.setEnv) [kv | kv <- saved, lookup (fst kv) now /= Just (snd kv)]

-- | Write a UTF-8 file, relative to the jail directory.
createFile :: Jail -> FilePath -> Text -> IO FilePath
createFile (Jail dir) rel contents = do
  let path = dir </> rel
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (TE.encodeUtf8 contents)
  pure path

-- | Write raw bytes (e.g. deliberately invalid UTF-8).
createBinaryFile :: Jail -> FilePath -> BS.ByteString -> IO FilePath
createBinaryFile (Jail dir) rel contents = do
  let path = dir </> rel
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path contents
  pure path

-- | Run an action with another working directory (restored afterwards).
inDirectory :: Jail -> FilePath -> IO a -> IO a
inDirectory _ d act = bracket getCurrentDirectory setCurrentDirectory (\_ -> setCurrentDirectory d >> act)

-- | Create a directory (and parents) inside the jail.
createDirectory :: Jail -> FilePath -> IO FilePath
createDirectory (Jail dir) rel = do
  let path = dir </> rel
  createDirectoryIfMissing True path
  pure path

-- | Set an environment variable for the duration of the jail. (As with "System.Environment", setting the empty string unsets it.)
setEnv :: Jail -> String -> String -> IO ()
setEnv _ = Env.setEnv

-- | Unset an environment variable for the duration of the jail.
unsetEnv :: Jail -> String -> IO ()
unsetEnv _ = Env.unsetEnv

-- | Remove every environment variable (restored when the jail ends).
clearEnv :: Jail -> IO ()
clearEnv _ = Env.getEnvironment >>= mapM_ (Env.unsetEnv . fst)

-- | A sandbox for configuration tests (Rust: @figment::Jail@).
--
-- 'withJail' runs an action in a fresh temporary working directory with
-- a snapshot of the environment, and restores both afterwards, even on
-- exceptions. Jails are serialised process-wide (the working directory
-- and the environment are process-global), so jailed tests are safe
-- under a parallel test runner. Tests that do not use a jail should not
-- touch the working directory or the environment.
--
-- The t'Jail' handle is only valid inside the 'withJail' that created it:
-- using it afterwards, or starting a jail from inside a jail on the same
-- thread (which could never acquire the lock), throws an 'IOError' with an
-- explanation instead of misbehaving or hanging. Paths given to
-- 'createFile' and friends must stay inside the jail.
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

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, bracket_, finally, throwIO)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import qualified System.Environment as Env
import qualified System.Environment.Blank as Blank
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)

-- | A handle on the running jail.
data Jail = Jail
  { jailDirectory :: FilePath
    -- ^ the temporary directory (also the working directory)
  , jailAlive :: IORef Bool
  }

-- The one piece of global state: the lock serialising jails, and the
-- thread that currently holds it (so a nested jail fails fast instead of
-- waiting for itself). Standard top-level idiom; NOINLINE guarantees a
-- single instance.
data Globals = Globals
  { gLock :: MVar ()
  , gHolder :: IORef (Maybe ThreadId)
  }

globals :: Globals
globals = unsafePerformIO (Globals <$> newMVar () <*> newIORef Nothing)
{-# NOINLINE globals #-}

-- | Run an action in a fresh jail; see the module header.
withJail :: (Jail -> IO a) -> IO a
withJail act = do
  me <- myThreadId
  holder <- readIORef (gHolder globals)
  when (holder == Just me) $
    throwIO (userError "Figment.Jail.withJail: jails cannot be nested (this thread is already inside one)")
  withMVar (gLock globals) $ \() ->
    bracket_ (writeIORef (gHolder globals) (Just me)) (writeIORef (gHolder globals) Nothing) $
      withSystemTempDirectory "figment-jail" $ \dir -> do
        alive <- newIORef True
        bracket Env.getEnvironment restoreEnv $ \_ ->
          bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
            setCurrentDirectory dir
            act (Jail dir alive) `finally` writeIORef alive False
  where
    restoreEnv saved = do
      now <- Map.fromList <$> Env.getEnvironment
      let before = Map.fromList saved
      mapM_ Env.unsetEnv [k | k <- Map.keys now, not (k `Map.member` before)]
      -- 'Blank.setEnv' (unlike 'Env.setEnv') keeps a variable whose
      -- original value was the empty string instead of unsetting it
      mapM_ (\(k, v) -> Blank.setEnv k v True) [(k, v) | (k, v) <- saved, Map.lookup k now /= Just v]

-- | Fail unless the jail is still running.
live :: Jail -> IO ()
live j = do
  ok <- readIORef (jailAlive j)
  unless ok $ throwIO (userError "Figment.Jail: the Jail handle was used after its withJail returned")

-- | The path inside the jail, refusing anything that would leave it: a
-- relative path may not climb out with @..@, and an absolute path must
-- already be inside the jail directory (as the paths returned by
-- 'createFile' and 'createDirectory' are).
inside :: Jail -> FilePath -> IO FilePath
inside j rel = do
  live j
  let parts = splitDirectories rel
      root = splitDirectories (jailDirectory j)
      contained
        | isAbsolute rel = root `isPrefixOf` parts
        | otherwise = True
  unless (contained && ".." `notElem` parts) $
    throwIO (userError ("Figment.Jail: path escapes the jail: " ++ rel))
  pure (jailDirectory j </> rel)

-- | Write a UTF-8 file, relative to the jail directory.
createFile :: Jail -> FilePath -> Text -> IO FilePath
createFile j rel contents = do
  path <- inside j rel
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (TE.encodeUtf8 contents)
  pure path

-- | Write raw bytes (e.g. deliberately invalid UTF-8).
createBinaryFile :: Jail -> FilePath -> BS.ByteString -> IO FilePath
createBinaryFile j rel contents = do
  path <- inside j rel
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path contents
  pure path

-- | Run an action with another working directory, given relative to the
-- jail directory (restored afterwards).
inDirectory :: Jail -> FilePath -> IO a -> IO a
inDirectory j d act = do
  dir <- inside j d
  bracket getCurrentDirectory setCurrentDirectory (\_ -> setCurrentDirectory dir >> act)

-- | Create a directory (and parents) inside the jail.
createDirectory :: Jail -> FilePath -> IO FilePath
createDirectory j rel = do
  path <- inside j rel
  createDirectoryIfMissing True path
  pure path

-- | Set an environment variable for the duration of the jail. (As with "System.Environment", setting the empty string unsets it.)
setEnv :: Jail -> String -> String -> IO ()
setEnv j k v = live j >> Env.setEnv k v

-- | Unset an environment variable for the duration of the jail.
unsetEnv :: Jail -> String -> IO ()
unsetEnv j k = live j >> Env.unsetEnv k

-- | Remove every environment variable (restored when the jail ends).
clearEnv :: Jail -> IO ()
clearEnv j = live j >> Env.getEnvironment >>= mapM_ (Env.unsetEnv . fst)

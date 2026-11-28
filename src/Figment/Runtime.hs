-- | Configuration that can be reloaded while a program runs.
--
-- A t'Runtime' holds the most recent /successfully loaded and decoded/
-- configuration. 'reload' runs the providers again; if anything fails, the
-- previous configuration stays in force and the errors are reported, so a
-- bad edit to a file can never take a running service down. Readers take
-- an immutable 'snapshot' and never block; 'awaitChange' blocks until the
-- configuration actually changes, which is what a worker that must react
-- to new settings needs.
--
-- Figment does not watch files. Call 'reload' on a signal, a timer or a
-- file-system notification of your choosing.
--
-- > rt <- startRuntime (from (file toml "App.toml") <> from (prefixed "APP_")) extract
-- > cfg <- snapshotValue <$> snapshot rt   -- cheap, never blocks
-- > r <- reload rt                         -- Left errs: old config kept
module Figment.Runtime
  ( Runtime
  , Snapshot (..)
  , Version
  , startRuntime
  , snapshot
  , reload
  , lastReloadError
  , awaitChange
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

import Figment.Core
import Figment.Error
import Figment.Value (stripTags)

-- | Counts the configurations a runtime has held. It starts at 1 and grows
-- by one each time a reload produces a configuration with different
-- /data/ (provenance is ignored: moving a key in a file does not count).
type Version = Word64

-- | An immutable view of the configuration at one moment.
data Snapshot a = Snapshot
  { snapshotVersion :: !Version
  , snapshotValue :: a
    -- ^ the decoded configuration
  , snapshotLoaded :: Loaded
    -- ^ the loaded data it was decoded from, for provenance queries
  }

data State a = State
  { stSnap :: Snapshot a
  , stError :: Maybe ConfigErrors
  }

-- | A reloadable configuration of type @a@.
data Runtime a = Runtime
  { rtFigment :: Figment
  , rtDecode :: Loaded -> Either ConfigErrors a
  , rtLock :: MVar ()
    -- ^ serialises reloads, so concurrent callers cannot interleave
  , rtState :: TVar (State a)
  }

-- | Load and decode the configuration for the first time. Fails with every
-- error found; there is no previous configuration to fall back on.
startRuntime
  :: Figment
  -> (Loaded -> Either ConfigErrors a)
  -- ^ typically 'Figment.Extract.extract'
  -> IO (Either ConfigErrors (Runtime a))
startRuntime fig dec = do
  r <- loadAndDecode fig dec
  case r of
    Left es -> pure (Left es)
    Right (l, a) -> do
      lock <- newMVar ()
      st <- newTVarIO (State (Snapshot 1 a l) Nothing)
      pure (Right (Runtime fig dec lock st))

-- | The current configuration. Never blocks and never fails.
snapshot :: Runtime a -> IO (Snapshot a)
snapshot rt = stSnap <$> readTVarIO (rtState rt)

-- | Run the providers again.
--
-- * On success with different data, the new configuration replaces the old
--   one, the version increases and 'awaitChange' callers wake up.
-- * On success with the same data, nothing changes.
-- * On failure, the current configuration is kept, the errors are
--   returned and remembered ('lastReloadError').
reload :: Runtime a -> IO (Either ConfigErrors Version)
reload rt = withMVar (rtLock rt) $ \() -> do
  r <- loadAndDecode (rtFigment rt) (rtDecode rt)
  atomically $ do
    State old _ <- readTVar (rtState rt)
    case r of
      Left es -> do
        writeTVar (rtState rt) (State old (Just es))
        pure (Left es)
      Right (l, a) -> do
        let changed = dataOf l /= dataOf (snapshotLoaded old)
            next = if changed then Snapshot (snapshotVersion old + 1) a l else old
        writeTVar (rtState rt) (State next Nothing)
        pure (Right (snapshotVersion next))
  where
    dataOf = Map.map (Map.map stripTags) . loadedData

-- | The errors of the most recent 'reload', if it failed. Cleared by the
-- next successful reload.
lastReloadError :: Runtime a -> IO (Maybe ConfigErrors)
lastReloadError rt = stError <$> readTVarIO (rtState rt)

-- | Block until the configuration is newer than the given version, then
-- return it. Returns at once if it already is.
--
-- > loop v = do { s <- awaitChange rt v; apply (snapshotValue s); loop (snapshotVersion s) }
awaitChange :: Runtime a -> Version -> IO (Snapshot a)
awaitChange rt v = atomically $ do
  State s _ <- readTVar (rtState rt)
  if snapshotVersion s > v then pure s else retry

loadAndDecode :: Figment -> (Loaded -> Either ConfigErrors a) -> IO (Either ConfigErrors (Loaded, a))
loadAndDecode fig dec = do
  r <- load fig
  pure $ case r of
    Left es -> Left es
    Right l -> case dec l of
      Left es -> Left es
      Right a -> Right (l, a)

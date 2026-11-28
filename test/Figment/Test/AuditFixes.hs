{-# LANGUAGE OverloadedStrings #-}

-- | Regression tests for the defects found by the 0.3.0.0 source audit
-- (F-01 to F-05, with F-02 shown not to be a defect), and tests for
-- "Figment.Report" and "Figment.Runtime".
module Figment.Test.AuditFixes (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, finally, try)
import Data.Either (isLeft)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.Directory (createDirectory, createDirectoryLink, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Figment
import qualified Figment.Jail as Jail
import Figment.Merge (coalesce)
import Figment.Value (untagged)
import Figment.Path (fromSegments)
import qualified Figment.Provider.Env as Env
import Figment.Report
import Figment.Runtime

type Counts = Map.Map Text Int

tests :: TestTree
tests =
  testGroup
    "audit"
    [ testGroup "F-01 array container provenance" f01
    , testGroup "F-02 Unicode digits are text, not numbers" f02
    , testGroup "F-03 structured env/CLI values are bounded before they are built" f03
    , testGroup "F-04 path round trip" f04
    , testGroup "F-05 jail and symbolic links" f05
    , testGroup "Figment.Report" report
    , testGroup "Figment.Runtime" runtime
    ]

f01 :: [TestTree]
f01 =
  [ testCase "the array's root tag follows the merge order, as for dictionaries" $ do
      let tl = Tag Nothing Nothing (Just (Pos 1 1)) Nothing
          tr = Tag Nothing Nothing (Just (Pos 2 2)) Nothing
          l = Value tl (Array (Vector.fromList [Value untagged (Num (Finite 1))]))
          r = Value tr (Array (Vector.fromList [Value untagged (Num (Finite 2))]))
      valueTag (coalesce Join l r) @?= tl
      valueTag (coalesce Adjoin l r) @?= tl
      valueTag (coalesce Merge l r) @?= tr
      valueTag (coalesce Admerge l r) @?= tr
  ]

f02 :: [TestTree]
f02 =
  [ testCase "non-ASCII decimal digits in an environment value stay text" $ do
      let arabic = "\x661\x662\x663"
      l <- load (from (Env.fromList [("APP_A", arabic), ("APP_B", "[" <> arabic <> "]")] (prefixed "APP_")))
             >>= either (assertFailure . show) pure
      fmap valueNode (findValue (keyPath "a") l) @?= Just (String arabic)
      case fmap valueNode (findValue (keyPath "b") l) of
        Just (Num _) -> assertFailure "parsed as a number"
        _ -> pure ()
  , testCase "a non-ASCII digit is not an integer map key, and nothing throws" $ do
      l <- load (from (fromText json "{\"m\":{\"\x663\":\"a\"}}")) >>= either (assertFailure . show) pure
      case extractAt (keyPath "m") l :: Either ConfigErrors (Map.Map Int Text) of
        Left _ -> pure ()
        Right v -> assertFailure (show v)
  ]

f03 :: [TestTree]
f03 =
  [ testCase "an environment array with far too many separators is rejected" $ do
      lim <- either (assertFailure . show) pure (mkLimits 64 10)
      let big = "[" <> Text.replicate 200 "1," <> "1]"
      r <- load (withLimits lim (from (Env.fromList [("APP_L", big)] (prefixed "APP_"))))
      either (map errorKind . errorList) (const []) r @?= [TooManyNodes 10]
  , testCase "a legitimate float array just inside the limit is not rejected" $ do
      lim <- either (assertFailure . show) pure (mkLimits 64 1001)
      let xs = "[" <> Text.intercalate "," (replicate 1000 "1.5") <> "]"
      r <- load (withLimits lim (from (Env.fromList [("APP_L", xs)] (prefixed "APP_"))))
      assertBool (either (Text.unpack . renderErrors) (const "") r) (either (const False) (const True) r)
  ]

f04 :: [TestTree]
f04 =
  [ testCase "a path with a negative index renders and parses back" $ do
      let p = fromSegments [Key "a", Index (-1), Key "b"]
      renderPath p @?= "a[-1].b"
      parsePath (renderPath p) @?= Right p
  ]

f05 :: [TestTree]
f05 =
  [ testCase "a symbolic link inside the jail cannot lead outside it" $ Jail.withJail $ \j -> do
      let outside = Jail.jailDirectory j ++ "-outside"
      createDirectory outside
      (`finally` removeDirectoryRecursive outside) $ do
        linked <- try (createDirectoryLink outside (Jail.jailDirectory j </> "link")) :: IO (Either IOException ())
        case linked of
          Left _ -> pure () -- links are unavailable here (for example, Windows without the privilege)
          Right () -> do
            r <- try (Jail.createFile j "link/x.txt" "x") :: IO (Either IOException FilePath)
            assertBool "the write was refused" (isLeft r)
            escaped <- doesFileExist (outside </> "x.txt")
            assertBool "nothing was written outside" (not escaped)
  , testCase "ordinary nested paths still work" $ Jail.withJail $ \j -> do
      p <- Jail.createFile j "a/b/c.txt" "x"
      ok <- doesFileExist p
      assertBool "created" ok
  ]

report :: [TestTree]
report =
  [ testCase "every leaf is listed with its provider and native key, and no value" $ do
      l <- load (from (Env.fromList [("APP_DB__PORT", "5432"), ("APP_PASSWORD", "hunter2")] (prefixed "APP_" & Env.split "__")))
             >>= either (assertFailure . show) pure
      map (renderPath . entryPath) (sources l) @?= ["db.port", "password"]
      map entryNative (sources l) @?= [Just "APP_DB__PORT", Just "APP_PASSWORD"]
      map entryKind (sources l) @?= ["number", "string"]
      let out = renderSources l
      assertBool (Text.unpack out) ("APP_DB__PORT" `Text.isInfixOf` out)
      assertBool "no values" (not ("hunter2" `Text.isInfixOf` out || "5432" `Text.isInfixOf` out))
  , testCase "a hostile key cannot forge extra lines" $ do
      l <- load (from (Env.fromList [("APP_A\nB", "1")] (prefixed "APP_")))
             >>= either (assertFailure . show) pure
      length (Text.lines (renderSources l)) @?= 1
  ]

runtime :: [TestTree]
runtime =
  [ testCase "reload keeps the last good configuration and reports the errors" $ Jail.withJail $ \j -> do
      path <- Jail.createFile j "c.json" "{\"n\":1}"
      rt <- start path
      s1 <- snapshot rt
      snapshotVersion s1 @?= 1
      snapshotValue s1 @?= Map.fromList [("n", 1)]
      -- same data: no new version
      reload rt >>= \r -> either (assertFailure . show) (@?= 1) r
      _ <- Jail.createFile j "c.json" "{\"n\":2}"
      reload rt >>= \r -> either (assertFailure . show) (@?= 2) r
      -- a broken edit changes nothing
      _ <- Jail.createFile j "c.json" "{\"n\":"
      bad <- reload rt
      assertBool "reload failed" (isLeft bad)
      s2 <- snapshot rt
      (snapshotVersion s2, snapshotValue s2) @?= (2, Map.fromList [("n", 2)])
      lastReloadError rt >>= \e -> assertBool "error remembered" (isJust e)
      -- the next good edit clears it
      _ <- Jail.createFile j "c.json" "{\"n\":3}"
      reload rt >>= \r -> either (assertFailure . show) (@?= 3) r
      lastReloadError rt >>= \e -> assertBool "error cleared" (isNothing e)
  , testCase "awaitChange wakes when the configuration changes" $ Jail.withJail $ \j -> do
      path <- Jail.createFile j "c.json" "{\"n\":1}"
      rt <- start path
      _ <- forkIO $ do
        threadDelay 100000
        _ <- Jail.createFile j "c.json" "{\"n\":9}"
        _ <- reload rt
        pure ()
      r <- timeout 10000000 (awaitChange rt 1)
      case r of
        Nothing -> assertFailure "timed out"
        Just s -> snapshotValue s @?= Map.fromList [("n", 9)]
  , testCase "awaitChange returns at once for an older version" $ Jail.withJail $ \j -> do
      path <- Jail.createFile j "c.json" "{\"n\":1}"
      rt <- start path
      _ <- Jail.createFile j "c.json" "{\"n\":2}"
      _ <- reload rt
      r <- timeout 5000000 (awaitChange rt 1)
      fmap snapshotVersion r @?= Just 2
  , testCase "a configuration that fails at start is an error, not a runtime" $ Jail.withJail $ \j -> do
      path <- Jail.createFile j "c.json" "{\"n\":"
      r <- startRuntime (from (fileExact json path)) extractCounts
      assertBool "start failed" (either (const True) (const False) r)
  ]
  where
    start path = startRuntime (from (fileExact json path)) extractCounts >>= either (assertFailure . show) pure

extractCounts :: Loaded -> Either ConfigErrors Counts
extractCounts = extract

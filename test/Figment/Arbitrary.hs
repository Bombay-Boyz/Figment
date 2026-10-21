-- | QuickCheck generators for the §10 property table of the feasibility
-- study.
module Figment.Arbitrary
  ( genSchemaUniformTriple
  , genArbitraryTagged
  , genShallowTagged
  , genShallowRawJSON
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Test.QuickCheck

import Figment.Value

-- | A shallow, non-adversarial tree: depth ≤ 3.
genShallowTagged :: Gen Tagged
genShallowTagged = genValue 3
  where
    genValue :: Int -> Gen Tagged
    genValue 0 = genLeaf
    genValue d = frequency
      [ (1, genLeaf)
      , (2, genObj (d - 1))
      , (2, genArr (d - 1))
      ]

    genLeaf :: Gen Tagged
    genLeaf = Tagged <$> elements
      [ VNull
      , VBool True
      , VBool False
      , VNum 1
      , VNum 2
      , VStr "a"
      , VStr "b"
      ] <*> pure Merged

    genObj :: Int -> Gen Tagged
    genObj d = do
      n     <- chooseInt (0, 3)
      pairs <- vectorOf n $ do
        k <- elements [Key "a", Key "b", Key "c"]
        v <- genValue d
        pure (k, v)
      pure (Tagged (VObj (Map.fromList pairs)) Merged)

    genArr :: Int -> Gen Tagged
    genArr d = do
      n  <- chooseInt (0, 3)
      xs <- vectorOf n (genValue d)
      pure (Tagged (VArr xs) Merged)

-- | Shape of a schema-uniform tree. Top-level because Haskell 2010 does
-- not allow local data declarations in 'where' blocks.
data Shape
  = SNull
  | SBool
  | SNum
  | SStr
  | SObj [(Key, Shape)]
  | SArr [Shape]

-- | Three trees with identical *shape* at every reachable path but
-- independent leaf payloads. This is the schema-uniformity hypothesis
-- of Lemma 3, enforced by the generator.
genSchemaUniformTriple :: Gen (Tagged, Tagged, Tagged)
genSchemaUniformTriple = do
  shape <- genShape 3
  pure ( fill shape 1, fill shape 2, fill shape 3 )
  where
    genShape :: Int -> Gen Shape
    genShape 0 = elements [SNull, SBool, SNum, SStr]
    genShape d = frequency
      [ (4, elements [SNull, SBool, SNum, SStr])
      , (3, do
           n   <- chooseInt (0, 2)
           ks  <- vectorOf n (elements [Key "a", Key "b", Key "c"])
           shs <- traverse (const (genShape (d - 1))) ks
           pure (SObj (zip ks shs)))
      , (3, do
           n   <- chooseInt (0, 2)
           shs <- vectorOf n (genShape (d - 1))
           pure (SArr shs))
      ]

fill :: Shape -> Int -> Tagged
fill sh seed = Tagged (go sh) Merged
  where
    go :: Shape -> Value
    go SNull = VNull
    go SBool = VBool (even seed)
    go SNum  = VNum (fromIntegral seed)
    go SStr  = VStr (Text.pack (show seed))
    go (SObj kvs) = VObj (Map.fromList [(k, fill s (seed + 1)) | (k, s) <- kvs])
    go (SArr ss)  = VArr (zipWith (\i s -> fill s (seed * 10 + i)) [1..] ss)

-- | Unrestricted tree generator (currently same as shallow, sufficient
-- to produce the Arr/Obj/Arr shape-mismatch counterexample for the
-- necessity test).
genArbitraryTagged :: Gen Tagged
genArbitraryTagged = genShallowTagged

-- | RawJSON generator used by the provenance property.
genShallowRawJSON :: Gen RawJSON
genShallowRawJSON = go (3 :: Int)
  where
    go :: Int -> Gen RawJSON
    go 0 = frequency
      [ (1, pure RawNull)
      , (1, pure (RawBool True))
      , (1, pure (RawNum 1))
      , (1, pure (RawStr "x"))
      ]
    go d = frequency
      [ (1, go 0)
      , (2, do
           n  <- chooseInt (0, 2)
           fs <- vectorOf n $ do
             k <- elements ["a", "b"]
             v <- go (d - 1)
             pure (k, v)
           pure (RawObj fs))
      , (2, do
           n  <- chooseInt (0, 2)
           xs <- vectorOf n (go (d - 1))
           pure (RawArr xs))
      ]

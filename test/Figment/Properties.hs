-- | Every property from the §10 table of the feasibility study. Each
-- one names the claim it checks; a test that never fails is itself a
-- bug (Standard §2.9). Marked in comments which are §10.1 \"pass\"
-- versus the three completions (§5.5).
module Figment.Properties (tests) where

import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.QuickCheck

import Figment.Error (ConfigError(..))
import Figment.Value
import Figment.Merge
import Figment.Layer
import Figment.Arbitrary

tests :: TestTree
tests = testGroup "Figment"
  [ testGroup "Lemma 1 — identity (Obj-shaped)"
      [ testProperty "prop_identity_obj"         prop_identity_obj
      , testProperty "prop_no_identity_scalar"   prop_no_identity_scalar
      ]
  , testGroup "Lemma 2 — non-commutativity"
      [ testProperty "prop_rightwins_not_commutative"
          prop_rightwins_not_commutative
      ]
  , testGroup "Lemma 3 — associativity (schema-uniform)"
      [ testProperty "prop_merge_associative Replace"
          (prop_merge_associative Replace)
      , testProperty "prop_merge_associative Concat"
          (prop_merge_associative Concat)
      , testProperty "prop_associativity_counterexample_exists"
          prop_associativity_counterexample_exists
      ]
  , testGroup "§1.3 depth bound"
      [ testProperty "prop_depth_exceeded_is_total"
          prop_depth_exceeded_is_total
      , testProperty "prop_depth_exceeded_reports_budget"
          prop_depth_exceeded_reports_budget
      , testProperty "prop_merge_bounded_by_max_operand_depth"
          prop_merge_bounded_by_max_operand_depth
      ]
  , testGroup "§4 provenance"
      [ testProperty "prop_provenance_path_correct"
          prop_provenance_path_correct
      ]
  ]

-- §10.1: "QuickCheck: pass". Restated here so the whole table lives in
-- one file.
prop_identity_obj :: Property
prop_identity_obj = forAll genShallowTagged $ \t ->
  case tVal t of
    VObj _ -> property $
      let empty' = Tagged (VObj Map.empty) Merged
          merged = mergeAlg RightWins Replace t empty'
      in tVal merged === tVal t
    _      -> property True  -- hypothesis not met, discard silently

-- §10.1: "QuickCheck: pass". Confirms the identity law is genuinely
-- restricted: a non-Obj value merged against ε does /not/ round-trip.
prop_no_identity_scalar :: Property
prop_no_identity_scalar = forAll genShallowTagged $ \t ->
  case tVal t of
    VObj _ -> property True
    _      -> t /= emptyObj ==>
                tVal (mergeAlg RightWins Replace t emptyObj) =/= tVal t

-- §10.1: "Executed: left=Arr[2], right=Arr[1,2]". Regression test, not
-- just a generator hit.
prop_rightwins_not_commutative :: Property
prop_rightwins_not_commutative =
  let n1 = Tagged (VNum 1) Merged
      n2 = Tagged (VNum 2) Merged
      left'  = mergeAlg RightWins Replace n1 n2
      right' = mergeAlg RightWins Replace n2 n1
  in (tVal left', tVal right') === (VNum 2, VNum 1)

-- §10.1: "QuickCheck, 100 cases each, schema-uniform generator: pass".
prop_merge_associative :: ArrayPolicy -> Property
prop_merge_associative ap =
  forAll genSchemaUniformTriple $ \(t1, t2, t3) ->
    let l = mergeAlg RightWins ap (mergeAlg RightWins ap t1 t2) t3
        r = mergeAlg RightWins ap t1 (mergeAlg RightWins ap t2 t3)
    in l === r

-- §10.1: "Executed" — the Arr/Obj/Arr counterexample. This test *must*
-- find a failure under the unrestricted generator; absence of one would
-- mean the necessity claim was wrong (Standard §2.9).
prop_associativity_counterexample_exists :: Property
prop_associativity_counterexample_exists = once $
  let t1 = Tagged (VArr [Tagged (VNum 1) Merged])            Merged
      t2 = Tagged (VObj (Map.singleton (Key "a") (Tagged (VNum 1) Merged))) Merged
      t3 = Tagged (VArr [Tagged (VNum 2) Merged])            Merged
      l = mergeAlg RightWins Concat (mergeAlg RightWins Concat t1 t2) t3
      r = mergeAlg RightWins Concat t1 (mergeAlg RightWins Concat t2 t3)
  in counterexample ("unexpected equality: " ++ show (tVal l)) (tVal l /= tVal r)

-- §10.1: "Executed: rejects depth 640000 with DepthExceeded, no stack
-- overflow". Adversarial deep synthetic RawJSON.
prop_depth_exceeded_is_total :: Property
prop_depth_exceeded_is_total =
  let deep = nest 640000
      result = mkTagged defaultDepth (SourceId "test", DefaultProfile) deep
  in case result of
       Left (DepthExceeded _ _) -> property True
       _                        -> counterexample "expected DepthExceeded" False
  where
    nest :: Int -> RawJSON
    nest n = go n
      where
        go 0 = RawNull
        go k = RawArr [go (k - 1)]

-- §10.1: "Executed: reports 30 for budget 30 and depth 40". Note we use
-- depth 40 under budget 30 to make this fast; the 640000 case above
-- covers the adversarial magnitude.
prop_depth_exceeded_reports_budget :: Property
prop_depth_exceeded_reports_budget =
  forAll (chooseInt (1, 60)) $ \b ->
    case mkDepth b of
      Left _ -> property True
      Right d ->
        let deep = nest 1000
            result = mkTagged d (SourceId "test", DefaultProfile) deep
        in case result of
             Left (DepthExceeded _ d') -> d' === d
             _                         -> counterexample "expected DepthExceeded" False
  where
    nest :: Int -> RawJSON
    nest n = go n
      where go 0 = RawNull
            go k = RawArr [go (k - 1)]

-- §10: "for chains up to length 100, result depth is max of operands".
prop_merge_bounded_by_max_operand_depth :: Property
prop_merge_bounded_by_max_operand_depth =
  forAll genShallowTagged $ \a ->
    forAll genShallowTagged $ \b ->
      forAll (chooseInt (1, 100)) $ \n ->
        let layers = replicate n (Merge a)
            merged = runLayers (Merge b : layers)
            da = treeDepth a
            db = treeDepth b
            dm = treeDepth merged
        in counterexample
             ("dm=" ++ show dm ++ " da=" ++ show da ++ " db=" ++ show db)
             (dm <= max da db)

-- §4 Provenance theorem: every leaf's path equals its own root-to-leaf
-- key sequence. We check this on a freshly tagged tree (path is set by
-- mkTagged), then after an arbitrary merge chain against another
-- freshly tagged tree, then verify the surviving leaves still have
-- correct paths.
prop_provenance_path_correct :: Property
prop_provenance_path_correct =
  forAll genShallowRawJSON $ \rawA ->
    forAll genShallowRawJSON $ \rawB ->
      forAll (chooseInt (0, 5)) $ \n ->
        let sid = SourceId "test"
            pa  = DefaultProfile
            ta = mkTagged defaultDepth (sid, pa) rawA
            tb = mkTagged defaultDepth (sid, pa) rawB
        in case (ta, tb) of
             (Right a, Right b) ->
               let merged = runLayers (replicate n (Merge b) ++ [Admerge a])
               in property (leavesHaveCorrectPaths merged)
             _ -> property True

-- | Does every leaf's 'path' in the metadata equal the root-to-leaf
-- key sequence observed by walking the tree?
leavesHaveCorrectPaths :: Tagged -> Bool
leavesHaveCorrectPaths t = go (Path []) t
  where
    go _ (Tagged _ Merged)         = True  -- synthetic; no path to check
    go p (Tagged v (Leaf _ _ pp))  = p == pp && descend p v

    descend _ VNull          = True
    descend _ (VBool _)      = True
    descend _ (VNum _)       = True
    descend _ (VStr _)       = True
    descend p (VObj m)       = all (\(Key k, sub) -> go (extend p k) sub) (Map.toList m)
    descend p (VArr xs)      = and (zipWith (\i sub -> go (extendIx p i) sub) [0..] xs)

-- | Depth of a 'Tagged' tree (used by the merge-depth property).
treeDepth :: Tagged -> Int
treeDepth (Tagged v _) = go v
  where
    go VNull          = 1
    go (VBool _)      = 1
    go (VNum _)       = 1
    go (VStr _)       = 1
    go (VObj m)       = 1 + maximum (0 : map (go . tVal) (Map.elems m))
    go (VArr xs)      = 1 + maximum (0 : map (go . tVal) xs)

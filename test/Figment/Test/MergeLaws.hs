-- | The algebra of 'coalesce' and of 'Figment' sequencing.
module Figment.Test.MergeLaws (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Figment
import Figment.Merge (coalesce)
import Figment.Test.Arbitrary
import Figment.Value (stripTags)

tests :: TestTree
tests =
  testGroup
    "merge laws"
    [ testProperty "duality: Join a b == Merge b a (including tags)" $
        forAll (genValue 3) $ \a -> forAll (genValue 3) $ \b ->
          coalesce Join a b === coalesce Merge b a
    , testProperty "idempotence of Merge and Join (data)" $
        forAll (genValue 3) $ \v ->
          stripTags (coalesce Merge v v) === stripTags v
            .&&. stripTags (coalesce Join v v) === stripTags v
    , testProperty "empty dictionary is a left and right identity on dictionaries" $
        forAll (genDict 3) $ \v -> conjoin
          [ stripTags (coalesce o (mkEmpty) v) === stripTags v .&&. stripTags (coalesce o v mkEmpty) === stripTags v
          | o <- [minBound .. maxBound] ]
    , testProperty "associative on same-shape trees, every order" $
        forAll genSameShape $ \(a, b, c) -> conjoin
          [ coalesce o (coalesce o a b) c === coalesce o a (coalesce o b c) | o <- [minBound .. maxBound] ]
    , testCase "not associative in general (why Value has no Semigroup)" $ do
        let a = dictV [("x", num 1)]
            b = num 5
            c = dictV [("y", num 2)]
        assertBool "counterexample" $
          stripTags (coalesce Merge (coalesce Merge a b) c) /= stripTags (coalesce Merge a (coalesce Merge b c))
    , testCase "adjoin/admerge concatenate arrays; merge/join replace" $ do
        let a = arr [num 1]
            b = arr [num 2]
        stripTags (coalesce Adjoin a b) @?= arr [num 1, num 2]
        stripTags (coalesce Admerge a b) @?= arr [num 1, num 2]
        stripTags (coalesce Merge a b) @?= arr [num 2]
        stripTags (coalesce Join a b) @?= arr [num 1]
    , testCase "container tags follow Rust: join keeps left, merge keeps right" $ do
        let tl = Tag Nothing Nothing (Just (Pos 1 1)) Nothing
            tr = Tag Nothing Nothing (Just (Pos 2 2)) Nothing
            l = Value tl (Dict Map.empty)
            r = Value tr (Dict Map.empty)
        valueTag (coalesce Join l r) @?= tl
        valueTag (coalesce Merge l r) @?= tr
    , testProperty "Figment is a monoid (observationally: same loaded data)" $
        forAll (genDict 2) $ \a -> forAll (genDict 2) $ \b -> forAll (genDict 2) $ \c ->
          ioProperty $ do
            let fa = from (Const a)
                fb = merge (Const b) new
                fc = join (Const c) new
            l1 <- loadedPlain ((fa <> fb) <> fc)
            l2 <- loadedPlain (fa <> (fb <> fc))
            l3 <- loadedPlain (new <> fa)
            l4 <- loadedPlain (fa <> new)
            l5 <- loadedPlain fa
            pure (l1 === l2 .&&. l3 === l5 .&&. l4 === l5)
    ]
  where
    mkEmpty = Value (Tag Nothing Nothing Nothing Nothing) (Dict Map.empty)
    num :: Int -> Value
    num = Value (Tag Nothing Nothing Nothing Nothing) . Num . Finite . fromIntegral
    arr = Value (Tag Nothing Nothing Nothing Nothing) . Array . Vector.fromList
    dictV = Value (Tag Nothing Nothing Nothing Nothing) . Dict . Map.fromList

-- | A provider returning a fixed dictionary.
newtype Const = Const Value

instance Provider Const where
  metadata _ = named "const"
  provide _ (Const v) = pure $ case valueNode v of
    Dict d -> Right (Map.singleton Default d)
    _ -> Right Map.empty

loadedPlain :: Figment -> IO (Maybe Value)
loadedPlain f = either (const Nothing) (Just . stripTags . selected) <$> load f

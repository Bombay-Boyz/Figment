-- | Generators for configuration trees.
module Figment.Test.Arbitrary
  ( genValue
  , genDict
  , genSameShape
  , Shape (..)
  , genShape
  , fill
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Test.QuickCheck

import Figment

genKey :: Gen Text.Text
genKey = elements ["a", "b", "c", "d"]

genScalar :: Gen Node
genScalar =
  oneof
    [ pure Null
    , Bool <$> arbitrary
    , Num . Finite . fromIntegral <$> chooseInt (-3, 3)
    , String . Text.pack <$> elements ["x", "y", ""]
    ]

-- | Arbitrary values, with arbitrary (distinguishable) tags.
genValue :: Int -> Gen Value
genValue d = do
  tag <- genTag
  n <-
    if d <= 0
      then genScalar
      else
        frequency
          [ (2, genScalar)
          , (2, Array . Vector.fromList <$> (chooseInt (0, 3) >>= \k -> vectorOf k (genValue (d - 1))))
          , (3, Dict . Map.fromList <$> (chooseInt (0, 3) >>= \k -> vectorOf k ((,) <$> genKey <*> genValue (d - 1))))
          ]
  pure (Value tag n)

genTag :: Gen Tag
genTag = do
  i <- chooseInt (0, 5)
  pure (Tag Nothing Nothing (Just (Pos i i)) Nothing)

genDict :: Int -> Gen Value
genDict d = do
  tag <- genTag
  kvs <- chooseInt (0, 4) >>= \k -> vectorOf k ((,) <$> genKey <*> genValue (d - 1))
  pure (Value tag (Dict (Map.fromList kvs)))

data Shape = SScalar | SArr [Shape] | SDict [(Text.Text, Shape)]
  deriving stock (Show)

genShape :: Int -> Gen Shape
genShape 0 = pure SScalar
genShape d =
  frequency
    [ (2, pure SScalar)
    , (1, SArr <$> (chooseInt (0, 2) >>= \k -> vectorOf k (genShape (d - 1))))
    , (3, SDict . Map.toList . Map.fromList <$> (chooseInt (0, 3) >>= \k -> vectorOf k ((,) <$> genKey <*> genShape (d - 1))))
    ]

-- | A value of the given shape with arbitrary leaves and tags.
fill :: Shape -> Gen Value
fill sh = do
  tag <- genTag
  case sh of
    SScalar -> Value tag <$> genScalar
    SArr xs -> Value tag . Array . Vector.fromList <$> traverse fill xs
    SDict kvs -> Value tag . Dict . Map.fromList <$> traverse (\(k, s) -> (,) k <$> fill s) kvs

genSameShape :: Gen (Value, Value, Value)
genSameShape = do
  sh <- genShape 3
  (,,) <$> fill sh <*> fill sh <*> fill sh

-- | A tiny state/error monad shared by every parser that builds a
-- t'Figment.Value.Value' from untrusted input: it enforces 'Limits' while the tree is
-- being built, so a hostile source is rejected before it is
-- materialised.
module Figment.Internal.Budget
  ( Budget
  , runBudget
  , spend
  , enterContainer
  , throwB
  , askLimits
  , checkBytes
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Figment.Error
import Figment.Limits
import Figment.Metadata (Pos)
import Figment.Path (Path)

newtype Budget a = Budget {unBudget :: Limits -> Int -> Either ConfigError (a, Int)}

instance Functor Budget where
  fmap f (Budget g) = Budget $ \l n -> case g l n of
    Left e -> Left e
    Right (a, n') -> Right (f a, n')

instance Applicative Budget where
  pure a = Budget $ \_ n -> Right (a, n)
  Budget f <*> Budget g = Budget $ \l n -> case f l n of
    Left e -> Left e
    Right (h, n') -> case g l n' of
      Left e -> Left e
      Right (a, n'') -> Right (h a, n'')

instance Monad Budget where
  Budget g >>= k = Budget $ \l n -> case g l n of
    Left e -> Left e
    Right (a, n') -> unBudget (k a) l n'

runBudget :: Limits -> Budget a -> Either ConfigError a
runBudget l (Budget g) = fst <$> g l (maxNodes l)

askLimits :: Budget Limits
askLimits = Budget $ \l n -> Right (l, n)

throwB :: ConfigError -> Budget a
throwB e = Budget $ \_ _ -> Left e

-- | Account for @k@ values at the given location.
spend :: Int -> Path -> Maybe Pos -> Budget ()
spend k p pos = Budget $ \l n ->
  let n' = n - k
  in if n' < 0
       then Left (withPos pos (atPath p (configError (TooManyNodes (maxNodes l)))))
       else Right ((), n')

-- | Check that a container at nesting level @depth@ (root = 1) is
-- allowed.
enterContainer :: Int -> Path -> Maybe Pos -> Budget ()
enterContainer depth p pos = Budget $ \l n ->
  if depth > maxDepth l
    then Left (withPos pos (atPath p (configError (DepthExceeded (maxDepth l)))))
    else Right ((), n)

-- | Reject a source larger than 'maxBytes' before anything is parsed.
checkBytes :: Limits -> ByteString -> Either ConfigError ()
checkBytes lim bs
  | BS.length bs > maxBytes lim = Left (configError (TooManyBytes (maxBytes lim)))
  | otherwise = Right ()

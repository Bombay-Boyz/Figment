-- | Turning flat @(key path, value)@ pairs (environment variables, CLI
-- arguments, keyed defaults) into a nested dictionary, reporting every
-- conflict instead of silently dropping data.
module Figment.Internal.Nest
  ( Duplicates (..)
  , nest
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Figment.Error
import Figment.Path
import Figment.Value

-- | What to do when two entries set exactly the same key.
data Duplicates
  = LastWins   -- ^ later entries override earlier ones (CLI convention)
  | RejectDuplicates -- ^ report 'DuplicateKey'

data Entry
  = Leaf !Text !Value
  | Branch !Text !(Map Text Entry)

-- | @nest policy entries@, where each entry is
-- @(original name, key segments, value)@. The original name (e.g.
-- @APP_DB\__HOST@) is used in conflict messages.
nest :: Duplicates -> [(Text, [Text], Value)] -> Either ConfigErrors Dict
nest policy entries =
  let (tree, errs) = foldl' step (Map.empty, []) entries
  in case fromErrorList (reverse errs) of
       Just es -> Left es
       Nothing -> Right (Map.map toValue tree)
  where
    step (tree, errs) (name, ks, v) = case insert root name ks v tree of
      Left e -> (tree, e : errs)
      Right tree' -> (tree', errs)

    insert _ _ [] _ tree = Right tree
    insert path name [k] v tree = case Map.lookup k tree of
      Nothing -> Right (Map.insert k (Leaf name v) tree)
      Just (Leaf other _) -> case policy of
        LastWins -> Right (Map.insert k (Leaf name v) tree)
        RejectDuplicates ->
          Left (atPath (path /. k) (configError (DuplicateKey (other <> " and " <> name))))
      Just (Branch other _) ->
        Left (atPath (path /. k) (configError (KeyConflict name other)))
    insert path name (k : ks) v tree = case Map.lookup k tree of
      Nothing -> do
        sub <- insert (path /. k) name ks v Map.empty
        Right (Map.insert k (Branch name sub) tree)
      Just (Branch first sub) -> do
        sub' <- insert (path /. k) name ks v sub
        Right (Map.insert k (Branch first sub') tree)
      Just (Leaf other _) ->
        Left (atPath (path /. k) (configError (KeyConflict other name)))

    toValue (Leaf _ v) = v
    toValue (Branch _ m) = mkValue (Dict (Map.map toValue m))

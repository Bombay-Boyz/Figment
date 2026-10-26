-- | Providers, figments, and loading.
--
-- A t'Figment' is a pure /description/: an ordered sequence of providers,
-- each with a combination 'Order'. Building one performs no IO; 'load'
-- runs every provider, collects every failure, and combines the results.
--
-- Unlike the value-level merge, sequencing figments is a lawful
-- 'Monoid' (it is the free monoid on steps), so library defaults and
-- application overrides compose with '<>'.
module Figment.Core
  ( -- * Providers
    Provider (..)
    -- * Figments
  , Figment
  , new
  , from
  , merge
  , join
  , adjoin
  , admerge
  , combine
  , select
  , withLimits
  , figmentLimits
  , figmentSources
    -- * Loading
  , Loaded
  , load
  , loadedProfile
  , loadedData
  , loadedSources
  , loadedLimits
  , selected
  , reselect
  , profiles
  , findValue
  , contains
  , findMetadata
  , focus
  ) where

import Control.Exception (SomeAsyncException, SomeException (..), displayException, fromException, throwIO, try)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Last (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, SrcLoc, callStack, getCallStack)

import Figment.Error
import Figment.Limits
import Figment.Merge
import Figment.Metadata
import Figment.Path
import Figment.Profile
import Figment.Value

-- | A source of configuration.
--
-- Implementations return one dictionary per profile. Values may be left
-- untagged: 'load' fills in this provider's 'metadata' and the profile
-- for every value that has none. A provider that learns better metadata
-- while reading (e.g. the resolved path of a searched-for file) may tag
-- values itself; existing tags are never overwritten.
--
-- 'provide' must enforce the given 'Limits' on any untrusted input and
-- should report failures as 'Left'. As a safety net, 'load' converts any
-- synchronous exception that escapes into an 'IoError'.
class Provider p where
  metadata :: p -> Metadata
  provide :: Limits -> p -> IO (Either ConfigErrors (Map Profile Dict))
  -- | A profile this provider asks to be selected, if any.
  providerProfile :: p -> Maybe Profile
  providerProfile _ = Nothing

data Step = Step
  { stepOrder :: !Order
  , stepMeta :: !Metadata
  , stepProfile :: !(Maybe Profile)
  , stepRun :: Limits -> IO (Either ConfigErrors (Map Profile Dict))
  }

-- | A configuration description.
data Figment = Figment
  { figSteps :: !(Seq Step)
  , figSelect :: !(Last Profile)
  , figLimitsOpt :: !(Last Limits)
  }

-- | Steps concatenate; for 'select' and 'withLimits' the right-most
-- setting wins. Associative with identity 'new' by construction.
instance Semigroup Figment where
  Figment a s l <> Figment b s' l' = Figment (a <> b) (s <> s') (l <> l')

instance Monoid Figment where
  mempty = new

-- | The empty figment.
new :: Figment
new = Figment Seq.empty (Last Nothing) (Last Nothing)

-- | A figment with a single provider.
from :: (HasCallStack, Provider p) => p -> Figment
from p = addStep Merge p new

-- | Add a provider whose values override existing ones.
merge :: (HasCallStack, Provider p) => p -> Figment -> Figment
merge = addStep Merge

-- | Add a provider whose values only fill gaps.
join :: (HasCallStack, Provider p) => p -> Figment -> Figment
join = addStep Join

-- | Like 'join', but arrays are concatenated.
adjoin :: (HasCallStack, Provider p) => p -> Figment -> Figment
adjoin = addStep Adjoin

-- | Like 'merge', but arrays are concatenated.
admerge :: (HasCallStack, Provider p) => p -> Figment -> Figment
admerge = addStep Admerge

-- | Add a provider with an explicit order.
combine :: (HasCallStack, Provider p) => Order -> p -> Figment -> Figment
combine = addStep

addStep :: (HasCallStack, Provider p) => Order -> p -> Figment -> Figment
addStep o p fig = fig {figSteps = figSteps fig Seq.|> step}
  where
    meta0 = metadata p
    meta = case (metaAddedAt meta0, callerLoc) of
      (Nothing, Just l) -> withAddedAt l meta0
      _ -> meta0
    step = Step o meta (providerProfile p) (\lim -> provide lim p)

-- | The outermost call site outside this module (the user's @merge@).
callerLoc :: HasCallStack => Maybe SrcLoc
callerLoc = case reverse (getCallStack callStack) of
  ((_, l) : _) -> Just l
  [] -> Nothing

-- | Select the profile to extract. An explicit selection always wins
-- over profile hints from providers.
select :: Profile -> Figment -> Figment
select p fig = fig {figSelect = Last (Just p)}

-- | Override the parser limits for every provider in this figment.
withLimits :: Limits -> Figment -> Figment
withLimits l fig = fig {figLimitsOpt = Last (Just l)}

-- | The parser limits in effect ('defaultLimits' unless overridden).
figmentLimits :: Figment -> Limits
figmentLimits = fromMaybe defaultLimits . getLast . figLimitsOpt

-- | Metadata of every provider, in order.
figmentSources :: Figment -> [Metadata]
figmentSources = map stepMeta . toList . figSteps

-- | A loaded configuration: every provider has been read and combined.
data Loaded = Loaded
  { loadedProfile :: !Profile
    -- ^ the selected profile
  , loadedData :: !(Map Profile Dict)
    -- ^ the combined data of every profile
  , loadedSources :: ![Metadata]
    -- ^ every provider, in order
  , loadedLimits :: !Limits
    -- ^ the limits the sources were read with
  }

-- | Run every provider, in order. Failures do not stop the load: all of
-- them are collected and reported together.
load :: Figment -> IO (Either ConfigErrors Loaded)
load fig = do
  let lim = figmentLimits fig
      steps = toList (figSteps fig)
  results <- traverse (runStep lim) steps
  let errs = [e | Left e <- results]
      oks = [(s, d) | (s, Right d) <- zip steps results]
      combined = foldl' (\acc (s, d) -> coalesceData (stepOrder s) acc (retagData (stepMeta s) d)) Map.empty oks
      hinted = foldl' (\acc s -> maybe acc (coalesceProfile (stepOrder s) acc) (stepProfile s)) Default steps
      chosen = fromMaybe hinted (getLast (figSelect fig))
  pure $ case errs of
    (e : es) -> Left (foldl' (<>) e es)
    [] -> Right (Loaded chosen combined (map stepMeta steps) lim)

runStep :: Limits -> Step -> IO (Either ConfigErrors (Map Profile Dict))
runStep lim s = do
  r <- try (stepRun s lim)
  case r of
    Right (Left es) -> pure (Left (fmap' es))
    Right ok -> pure ok
    Left (ex :: SomeException)
      | isJust (fromException ex :: Maybe SomeAsyncException) -> throwIO ex
      | otherwise ->
          pure (Left (single (withMetadata (Just (stepMeta s)) (configError (IoError (Text.pack (bare ex)))))))
  where
    -- attach this step's metadata to errors that have none
    fmap' es = case fromErrorList [maybe (e {errorMetadata = Just (stepMeta s)}) (const e) (errorMetadata e) | e <- errorList es] of
      Just es' -> es'
      Nothing -> es

-- | The exception's message without the backtrace GHC >= 9.10 appends.
bare :: SomeException -> String
bare (SomeException e) = displayException e

retagData :: Metadata -> Map Profile Dict -> Map Profile Dict
retagData m = Map.mapWithKey (\p d -> fmap (retag m p) d)

instance Provider Figment where
  metadata _ = named "Figment"
  provide _ fig = fmap loadedData <$> load fig
  providerProfile = getLast . figSelect

-- | The configuration of the selected profile: 'Default', then the
-- selected profile, then 'Global', each merged over the previous.
selected :: Loaded -> Value
selected l =
  let get p = Map.findWithDefault Map.empty p (loadedData l)
      prof = loadedProfile l
      base = get Default
      withSel = if prof == Default || prof == Global then base else coalesceDict Merge base (get prof)
      final = coalesceDict Merge withSel (get Global)
  in Value untagged {tagProfile = Just prof} (Dict final)

-- | Choose another profile without re-reading any source.
reselect :: Profile -> Loaded -> Loaded
reselect p l = l {loadedProfile = p}

-- | Every profile with data.
profiles :: Loaded -> [Profile]
profiles = Map.keys . loadedData

-- | The value at a path in the selected profile.
findValue :: Path -> Loaded -> Maybe Value
findValue p = lookupPath p . selected

-- | Is there a value at this path in the selected profile?
contains :: Path -> Loaded -> Bool
contains p = isJust . findValue p

-- | The provider that supplied the value at a path.
findMetadata :: Path -> Loaded -> Maybe Metadata
findMetadata p l = findValue p l >>= tagMeta . valueTag

-- | Re-root every profile at the given key path (Rust: @focus@).
-- Profiles in which the path is absent or not a dictionary become empty.
focus :: Path -> Loaded -> Loaded
focus p l = l {loadedData = Map.mapMaybe sub (loadedData l)}
  where
    sub d = case lookupPath p (mkValue (Dict d)) of
      Just (Value _ (Dict d')) -> Just d'
      _ -> Nothing

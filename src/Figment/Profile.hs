-- | Profiles: named slices of configuration ("semi-hierarchical"
-- configuration, as in Rust figment).
--
-- Every provider yields one dictionary per profile. On extraction, the
-- 'Default' profile is merged with the selected profile, and the
-- 'Global' profile is merged over both.
module Figment.Profile
  ( Profile (Default, Global)
  , profile
  , profileName
  , isCustom
  , profileFromEnv
  ) where

import Control.DeepSeq (NFData)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

-- | A configuration profile. Custom profile names are case-insensitive
-- (stored lower-cased), matching Rust figment. The 'Named' constructor
-- is not exported: use 'profile', which maps @"default"@ and
-- @"global"@ to the built-in profiles so there is exactly one
-- representation of each.
data Profile
  = Default
  | Global
  | Named !Text
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show Profile where
  showsPrec _ Default = showString "Default"
  showsPrec _ Global = showString "Global"
  showsPrec d (Named t) = showParen (d > 10) $ showString "profile " . shows t

instance IsString Profile where
  fromString = profile . Text.pack

-- | Smart constructor. Case-insensitive; surrounding whitespace is
-- ignored; the empty name is 'Default'.
profile :: Text -> Profile
profile raw = case Text.toLower (Text.strip raw) of
  "" -> Default
  "default" -> Default
  "global" -> Global
  t -> Named t

-- | The canonical (lower-case) name of a profile.
profileName :: Profile -> Text
profileName Default = "default"
profileName Global = "global"
profileName (Named t) = t

-- | Is this a user-defined profile (neither 'Default' nor 'Global')?
isCustom :: Profile -> Bool
isCustom (Named _) = True
isCustom _ = False

-- | Read a profile from an environment variable, falling back to the
-- given profile when it is unset or empty (Rust: @Profile::from_env_or@).
profileFromEnv :: String -> Profile -> IO Profile
profileFromEnv var fallback = do
  v <- lookupEnv var
  pure $ case v of
    Just s | not (null s) -> fromString s
    _ -> fallback

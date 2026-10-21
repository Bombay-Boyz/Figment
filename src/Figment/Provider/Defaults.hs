{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Default-values provider (§8.2 of the feasibility study). Renders a
-- caller-supplied default record into a 'RawJSON' tree via aeson's
-- ToJSON, then tags it. There is no file or environment read; the
-- values are already in memory.
module Figment.Provider.Defaults
  ( Defaults(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import Figment.Provider (Provider(..), Defaults(..))
import Figment.Provider.Json (fromAeson)
import Figment.Value
  ( SourceId(..), Profile(..), mkTagged )

instance Aeson.ToJSON a => Provider (Defaults a) where
  tagProvider d (Defaults def) =
    pure (mkTagged d (SourceId (Text.pack "defaults"), DefaultProfile)
                    (fromAeson (Aeson.toJSON def)))

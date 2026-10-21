{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module OkProvider where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Figment (composeChecked, Layer, EnvPrefixed)

data AppConfig = AppConfig
  deriving (Generic)
instance FromJSON AppConfig

_ = composeChecked @AppConfig @'[EnvPrefixed] ([] :: [Layer])

main :: IO ()
main = pure ()

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module OkProvider where

import Figment.Check (RequireGenericProvider)
import Figment.Provider (EnvPrefixed)

data AppConfig = AppConfig

checkIt :: RequireGenericProvider r ps => ()
checkIt = ()

_ = checkIt @AppConfig @'[EnvPrefixed]

main :: IO ()
main = pure ()

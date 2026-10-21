{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Figment.Check (RequireGenericProvider)

data AppConfig = AppConfig

checkIt :: RequireGenericProvider r ps => ()
checkIt = ()

_ = checkIt @AppConfig @'[]

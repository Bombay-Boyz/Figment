{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module NoProvider where

import Figment (composeChecked, Layer)

data AppConfig = AppConfig

_ = composeChecked @AppConfig @'[] ([] :: [Layer])

main :: IO ()
main = pure ()

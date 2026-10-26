-- Must fail: coalescing values is not associative, so Value has no
-- Semigroup instance (the lawful monoid is on Figment).
module NoValueSemigroup where

import Figment (Value)

combineValues :: Value -> Value -> Value
combineValues = (<>)

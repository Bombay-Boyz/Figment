module Figment.Error (ConfigError(..)) where

import Control.DeepSeq (NFData(rnf))
import Data.Text (Text)
import GHC.Generics (Generic)
import Figment.Prim (Path, Depth)

data ConfigError
  = SourceError    FilePath Text
  | DepthExceeded  Path Depth
  | DecodeError    Path Text
  | MissingKey     Path
  | ValidationError Path Text
  | ConfigurationError Text Text
  deriving (Eq, Show, Generic)
instance NFData ConfigError where
  rnf (SourceError fp t)    = rnf fp `seq` rnf t
  rnf (DepthExceeded pa d)  = rnf pa `seq` rnf d
  rnf (DecodeError pa t)    = rnf pa `seq` rnf t
  rnf (MissingKey pa)       = rnf pa
  rnf (ValidationError pa t)= rnf pa `seq` rnf t
  rnf (ConfigurationError a b) = rnf a `seq` rnf b

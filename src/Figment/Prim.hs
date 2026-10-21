-- | Primitive newtypes and the 'Depth' bound type — the only things
-- 'Figment.Error' and 'Figment.Value' must both see. Sits below both so
-- neither imports the other (Standard §2.6: dependency direction points
-- at the core).
--
-- Note: 'Depth(..)' is exported *here* so 'Figment.Value.mkTagged' can
-- decompose the budget. It is deliberately hidden from the public
-- façade ('Figment.hs'), which re-exports only the abstract 'Depth'
-- type (Standard §2.5, feasibility study §9).
module Figment.Prim
  ( Key(..)
  , SourceId(..)
  , Path(..)
  , Profile(..)
  , ProfileName(..)
  , Depth(..)
  , defaultDepth
  , extend
  , extendIx
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Text as Text

newtype Key      = Key      Text deriving (Eq, Ord, Show, Generic)
instance NFData Key where rnf (Key t) = rnf t
newtype SourceId = SourceId Text deriving (Eq, Ord, Show, Generic)
instance NFData SourceId where rnf (SourceId t) = rnf t
newtype Path     = Path     [Key] deriving (Eq, Ord, Show, Generic)
instance NFData Path where rnf (Path ks) = rnf ks

data Profile = DefaultProfile | NamedProfile ProfileName
  deriving (Eq, Show, Generic)
instance NFData Profile where
  rnf DefaultProfile     = ()
  rnf (NamedProfile pn)  = rnf pn

newtype ProfileName = ProfileName Text deriving (Eq, Ord, Show, Generic)
instance NFData ProfileName where rnf (ProfileName t) = rnf t

-- | A strictly positive recursion budget. 'Figment.Value.mkDepth' is
-- the public way to obtain one.
newtype Depth = UnsafeDepth Int deriving (Eq, Ord, Show, Generic)
instance NFData Depth where rnf (UnsafeDepth n) = rnf n

defaultDepth :: Depth
defaultDepth = UnsafeDepth 64

extend :: Path -> Text -> Path
extend (Path ks) k = Path (ks ++ [Key k])

extendIx :: Path -> Int -> Path
extendIx (Path ks) i = Path (ks ++ [Key (Text.pack (show i))])

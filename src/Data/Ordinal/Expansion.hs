{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Expansion 
  ( module Data.Ordinal.Expansion.Internal
  , pattern Expansion, pattern Lifted
  ) where

import qualified Data.Ordinal.Expansion.Internal as Internal
-- hide the dumb constructor and pattern from the external API
-- as they could be used to create invariant-breaking values
import Data.Ordinal.Expansion.Internal hiding (Expansion, Lifted)
-- but not the type
import Data.Ordinal.Expansion.Internal (Expansion())
import Data.Ordinal.Positive

pattern Expansion :: [(Expansion a, Positive a)] -> Expansion a
pattern Expansion ps <- Internal.Expansion ps

pattern Lifted :: a -> Expansion a
pattern Lifted a <- Internal.Lifted a

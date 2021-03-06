{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Ordinal.Expansion 
  ( module Data.Ordinal.Expansion.Internal
  , pattern Expansion, pattern Lifted
  ) where

import Data.Ordinal.Positive
import Data.Ordinal.Lens
import Data.Ordinal.Zero

import qualified Data.Ordinal.Expansion.Internal as Internal
-- hide the dumb constructor and pattern from the external API
-- as they could be used to create invariant-breaking values
import Data.Ordinal.Expansion.Internal hiding (pattern Expansion, pattern Lifted)

-- | smart constructor
pattern Expansion :: (Num (Expansion a)) => [(Expansion a, Positive a)] -> Expansion a
pattern Expansion ps <- Internal.Expansion ps where
  Expansion = sum . map (Internal.Expansion . return)

pattern Lifted :: HasZero a => a -> Expansion a
pattern Lifted a <- (toBase -> Just a) where
  Lifted = fromBase
  


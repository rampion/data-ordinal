{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Expansion 
  ( module Data.Ordinal.Expansion.Internal
  , pattern Expansion, pattern LiftedPositive
  ) where

import Data.Ordinal.Positive
import Data.Ordinal.Zero

import qualified Data.Ordinal.Expansion.Internal as Internal
-- hide the dumb constructor and pattern from the external API
-- as they could be used to create invariant-breaking values
import Data.Ordinal.Expansion.Internal hiding (pattern Expansion, pattern Lifted)

-- | smart constructor
pattern Expansion :: (Num (Expansion a)) => [(Expansion a, Positive a)] -> Expansion a
pattern Expansion ps <- Internal.Expansion ps where
  Expansion = sum . map (Internal.Expansion . return)

pattern LiftedPositive :: (HasZero (Expansion a)) => Positive a -> Expansion a
pattern LiftedPositive a = Internal.Expansion [(Zero, a)]

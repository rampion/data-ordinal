{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Positive
  ( module Data.Ordinal.Positive.Internal
  , pattern Positive
  ) where

import qualified Data.Ordinal.Positive.Internal as Internal
-- hide the dumb constructor and unsafe accessors from the external API
-- as they could be used to create invariant-breaking values
import Data.Ordinal.Positive.Internal hiding (pattern Positive, map, apply)

pattern Positive :: (Num a, Ord a) => a -> Positive a
pattern Positive a <- Internal.Positive a

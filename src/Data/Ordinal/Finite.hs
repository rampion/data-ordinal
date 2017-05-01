{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Finite
  ( module Data.Ordinal.Finite.Internal
  , pattern Finite
  ) where

import qualified Data.Ordinal.Finite.Internal as Internal
-- hide the dumb constructor and unsafe accessors from the external API as they
-- could be used to create invariant-breaking values
import Data.Ordinal.Finite.Internal hiding (pattern Finite, map, apply)

pattern Finite :: Integer -> Finite
pattern Finite a <- Internal.Finite a

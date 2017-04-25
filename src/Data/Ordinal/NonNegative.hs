{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.NonNegative
  ( module Data.Ordinal.NonNegative.Internal
  , pattern NonNegative
  ) where

import qualified Data.Ordinal.NonNegative.Internal as Internal
-- hide the dumb constructor and unsafe accessors from the external API
-- as they could be used to create invariant-breaking values
import Data.Ordinal.NonNegative.Internal hiding (NonNegative, map, apply)
-- but not the type
import Data.Ordinal.NonNegative.Internal (NonNegative())

pattern NonNegative :: a -> NonNegative a
pattern NonNegative a <- Internal.NonNegative a

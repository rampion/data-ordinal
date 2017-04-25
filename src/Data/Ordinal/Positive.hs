{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Positive
  ( module Data.Ordinal.Positive.Internal
  , pattern Positive
  ) where

import qualified Data.Ordinal.Positive.Internal as Internal
-- hide the dumb constructor and unsafe accessors from the external API
-- as they could be used to create invariant-breaking values
import Data.Ordinal.Positive.Internal hiding (Positive, map, apply)
-- but not the type
import Data.Ordinal.Positive.Internal (Positive())

pattern Positive :: a -> Positive a
pattern Positive a <- Internal.Positive a

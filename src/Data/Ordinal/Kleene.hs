{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module Data.Ordinal.Kleene 
  ( module Data.Ordinal.Kleene.Internal
  , pattern Wrap, pattern ViewKleene
  ) where

import qualified Data.Ordinal.Kleene.Internal as Internal
-- hide the dumb constructors from the external API
-- as they could be used to create invariant-breaking values
import Data.Ordinal.Kleene.Internal hiding (Wrap, ViewKleene)
-- but not the type
import Data.Ordinal.Kleene.Internal (ViewKleene())

pattern Wrap :: Kleene t (t b) -> Kleene t b
pattern Wrap k <- Internal.Wrap k

pattern ViewKleene :: IsKleene a t b -> a -> ViewKleene t b
pattern ViewKleene pf a <- Internal.ViewKleene pf a

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Ordinal.Kleene 
  ( module Data.Ordinal.Kleene.Internal
  , pattern Wrap
  ) where

import qualified Data.Ordinal.Kleene.Internal as Internal
import Data.Ordinal.Kleene.Internal
  ( Kleene(Pure)
  , Derived
  )

pattern Wrap :: Kleene t (t b) -> Kleene t b
pattern Wrap k <- Internal.Wrap k

class Derived a => ToKleene a t b where
  context :: Internal.Context a t b

instance Derived a => ToKleene a t a where
  context = Internal.Refl

instance (Derived (t a), ToKleene a t b) => ToKleene (t a) t b where
  context = Internal.Impl context

{-
view :: (forall a. Derived a => a -> x) -> Kleene t b -> x
view = undefined
-}

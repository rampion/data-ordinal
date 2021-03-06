{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
module Data.Ordinal.Kleene 
  ( I.Kleene(I.Point), pattern Lower, lower
  , I.Derived
  , IsKleene(..), toKleene, FromKleene(..), fromKleene
  ) where

import qualified Data.Ordinal.Kleene.Internal as I
import Data.Ordinal.Lens
import Data.Ordinal.Zero

-- | smart constructor
pattern Lower :: (LensBase t, I.Derived b) => I.Kleene t (t b) -> I.Kleene t b
pattern Lower k <- I.Lower k
  where Lower k = lower k

-- | smart constructor, simplifies if possible
lower :: (LensBase t, I.Derived b) => I.Kleene t (t b) -> I.Kleene t b
lower (I.Point (viewBase -> (b, Zero))) = I.Point b
lower k = I.Lower k

class (LensBase t, I.Derived a) => IsKleene a t b where
  context :: I.Context a t b

instance (LensBase t, I.Derived a) => IsKleene a t a where
  context = I.Reflexive

instance (LensBase t, I.Derived (t a), IsKleene a t b) => IsKleene (t a) t b where
  context = I.Inductive context

data FromKleene t b where
  FromKleene :: IsKleene a t b => a -> FromKleene t b

toKleene :: IsKleene a t b => a -> I.Kleene t b
toKleene = I.toKleene context

fromKleene :: forall t b. LensBase t => I.Kleene t b -> FromKleene t b
fromKleene k = I.fromKleene k $ \pf a -> isKleene pf $ FromKleene a where
  isKleene :: forall a x. I.Context a t b -> (IsKleene a t b => x) -> x
  isKleene I.Reflexive = id
  isKleene (I.Inductive pf) = isKleene pf

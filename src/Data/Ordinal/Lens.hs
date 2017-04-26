{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Ordinal.Lens where
import Data.Functor.Identity (Identity(..))

import Data.Ordinal.Finite
class LensFinite a where
  -- | manipulate the finite component of an ordinal number
  --
  -- e.g. if α = ω ^ β + c, where
  --  ω is the smallest transfinite ordinal
  --  β is any ordinal
  --  c is any finite ordinal
  -- then this should allow you to manipulate c
  lensFinite :: Functor f => (Finite -> f Finite) -> a -> f a

instance LensFinite Finite where
  lensFinite = id

instance (LensFinite a, Num a, Eq a, LensBase t) => LensFinite (t a) where
  lensFinite = lensBase . lensFinite

class LensBase t where
  -- | manipulate the base-a component of a ordinal number transformer
  -- 
  -- e.g. the largest value c s.t. α = β + c
  lensBase :: (Functor f, Num a, Eq a) => (a -> f a) -> t a -> f (t a)

fromBase :: (LensBase t, Num (t a), Num a, Eq a) => a -> t a
fromBase a = runIdentity $ lensBase (\_ -> Identity a) 0

-- | Invariant: 
--    > viewBase α = (a, α') =>
--    >   α' + fromBase a = α AND
--    >   viewBase α' = (0, α')
viewBase :: (LensBase t, Num a, Eq a) => t a -> (a, t a)
viewBase = lensBase (,0)

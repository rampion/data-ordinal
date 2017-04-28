{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.Ordinal.Kleene.Internal where 

-- import GHC.Exts (Constraint)
import Data.Ordinal.Zero
import Data.Ordinal.Lens

type Derived a = (HasZero a, Ord a, Num a)

-- think of Kleene and IsKleene it as lists of dictionaries in opposite order

data Kleene t b where
  Pure :: Derived b => b -> Kleene t b
  Wrap :: Derived b => Kleene t (t b) -> Kleene t b

peekKleene :: Kleene t b -> (Derived b => x) -> x
peekKleene (Pure _) x = x
peekKleene (Wrap _) x = x

instance Derived b => HasZero (Kleene t b) where
  isZero (Pure Zero) = True
  isZero _ = False
  zero = Pure Zero

instance LensBase t => Eq (Kleene t b) where
  j == k = case j `compare` k of EQ -> True ; _ -> False

instance LensBase t => Ord (Kleene t b) where
  compare = bicata compare

bicata :: LensBase t => (forall a. Derived a => a -> a -> x) -> Kleene t b -> Kleene t b -> x
bicata f (Wrap j) (Wrap k) = bicata f j k
bicata f (Pure a) (Wrap k) = k `peekKleene` bicata f (Pure $ fromBase a) k
bicata f (Wrap j) (Pure b) = j `peekKleene` bicata f j (Pure $ fromBase b)
bicata f (Pure a) (Pure b) = f a b

instance LensBase t => Num (Kleene t b) where
  -- j + k = fromViewKleene $ bicata (\a b -> toViewKleene $ a + b) j k
  Wrap j + Wrap k = Wrap $ j + k
  Pure a + Wrap k = peekKleene k $ Wrap $ Pure (fromBase a) + k
  Wrap j + Pure b = peekKleene j $ Wrap $ j + Pure (fromBase b)
  Pure a + Pure b = Pure $ a + b
  (*) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  
data IsKleene a t b where
  Refl :: Derived a => IsKleene a t a
  Impl :: Derived (t a) => IsKleene a t b -> IsKleene (t a) t b

data ViewKleene t b where
  ViewKleene :: IsKleene a t b -> a -> ViewKleene t b

instance Derived b => HasZero (ViewKleene t b) where
  isZero (ViewKleene Refl Zero) = True
  isZero _ = False
  zero = ViewKleene Refl Zero

{-
instance Eq (ViewKleene t b) where
  vj == vk = case vj `compare` vk of EQ -> True ; _ -> False

instance Ord (ViewKleene t b) where
-}
  
-- | smart constructor
toViewKleene :: forall a t b. (ToKleene a t b, LensBase t) => a -> ViewKleene t b
toViewKleene = simplify isKleene

simplify :: LensBase t => IsKleene a t b -> a -> ViewKleene t b
simplify (Impl pf@Refl) (viewBase -> (a, Zero)) = simplify pf a
simplify (Impl pf@(Impl _)) (viewBase -> (a, Zero)) = simplify pf a
simplify pf a = ViewKleene pf a

-- | smart constructor
toKleene :: forall a t b. (ToKleene a t b, LensBase t) => a -> Kleene t b
toKleene = fromViewKleene . toViewKleene

-- fromViewKleene & fromKleene traverse the list of dictionaries and reverse it

fromViewKleene :: forall t b. ViewKleene t b -> Kleene t b
fromViewKleene = check where
  check (ViewKleene pf@Refl a) = wrap pf (Pure a)
  check (ViewKleene pf@(Impl _) a) = wrap pf (Pure a)

  wrap :: forall x y. IsKleene x t y -> Kleene t x -> Kleene t y
  wrap (Impl pf@Refl) k = wrap pf (Wrap k)
  wrap (Impl pf@(Impl _)) k = wrap pf (Wrap k)
  wrap Refl k = k

fromKleene :: forall t b. Kleene t b -> ViewKleene t b
fromKleene = \k -> k `peekKleene` unwrap k Refl where
  unwrap :: forall x y. Kleene t x -> IsKleene x t y -> ViewKleene t y
  unwrap (Wrap k) pf = k `peekKleene` unwrap k (Impl pf)
  unwrap (Pure b) pf = ViewKleene pf b

class Derived a => ToKleene a t b where
  isKleene :: IsKleene a t b

instance Derived a => ToKleene a t a where
  isKleene = Refl

instance (Derived (t a), ToKleene a t b) => ToKleene (t a) t b where
  isKleene = Impl isKleene

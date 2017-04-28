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

data HasDerived a where
  QED :: Derived a => HasDerived a

peekI :: IsKleene a t b -> HasDerived a
peekI Refl = QED
peekI (Impl _) = QED

type Derived a = (HasZero a, Ord a, Num a)

-- think of Kleene and IsKleene it as lists of dictionaries in opposite order

data Kleene t b where
  Pure :: Derived b => b -> Kleene t b
  Wrap :: Derived b => Kleene t (t b) -> Kleene t b

peekK :: Kleene t b -> HasDerived b
peekK (Pure _) = QED
peekK (Wrap _) = QED

checkK :: Kleene t b -> (Derived b => x) -> x
checkK (peekK -> QED) = id

instance Derived b => HasZero (Kleene t b) where
  isZero (Pure Zero) = True
  isZero _ = False
  zero = Pure Zero

instance LensBase t => Eq (Kleene t b) where
  j == k = case j `compare` k of EQ -> True ; _ -> False

instance LensBase t => Ord (Kleene t b) where
  compare = bicata $ \(peekI -> QED) -> compare

bicata :: forall t b x. LensBase t => (forall a. IsKleene a t b -> a -> a -> x) -> Kleene t b -> Kleene t b -> x
bicata f = \j k -> k `checkK` loop Refl j k where
  loop :: IsKleene a t b -> Kleene t a -> Kleene t a -> x
  loop pf (Wrap j) (Wrap k) = k `checkK` loop (Impl pf) j k
  loop pf (Pure a) (Wrap k) = k `checkK` loop (Impl pf) (Pure $ fromBase a) k
  loop pf (Wrap j) (Pure b) = j `checkK` loop (Impl pf) j (Pure $ fromBase b)
  loop pf (Pure a) (Pure b) = f pf a b

op :: LensBase t => (forall a. Derived a => a -> a -> a) -> Kleene t b -> Kleene t b -> Kleene t b
op (#) j k = fromViewKleene $ bicata (\pf@(peekI -> QED) a b -> simplify pf $ a # b) j k 

instance LensBase t => Num (Kleene t b) where
  (+) = op (+)
  (*) = op (*)
  (-) = op (-) -- use minus instead?
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
simplify (Impl pf@(peekI -> QED)) (viewBase -> (a, Zero)) = simplify pf a
simplify pf a = ViewKleene pf a

-- | smart constructor
toKleene :: forall a t b. (ToKleene a t b, LensBase t) => a -> Kleene t b
toKleene = fromViewKleene . toViewKleene

-- fromViewKleene & fromKleene traverse the list of dictionaries and reverse it

fromViewKleene :: forall t b. ViewKleene t b -> Kleene t b
fromViewKleene = \(ViewKleene pf@(peekI -> QED) a) -> wrap pf (Pure a) where

  wrap :: forall x y. IsKleene x t y -> Kleene t x -> Kleene t y
  wrap (Impl pf@(peekI -> QED)) k = wrap pf (Wrap k)
  wrap Refl k = k

fromKleene :: forall t b. Kleene t b -> ViewKleene t b
fromKleene = \k@(peekK -> QED) ->  unwrap k Refl where
  unwrap :: forall x y. Kleene t x -> IsKleene x t y -> ViewKleene t y
  unwrap (Wrap k@(peekK -> QED)) pf = unwrap k (Impl pf)
  unwrap (Pure b) pf = ViewKleene pf b

class Derived a => ToKleene a t b where
  isKleene :: IsKleene a t b

instance Derived a => ToKleene a t a where
  isKleene = Refl

instance (Derived (t a), ToKleene a t b) => ToKleene (t a) t b where
  isKleene = Impl isKleene

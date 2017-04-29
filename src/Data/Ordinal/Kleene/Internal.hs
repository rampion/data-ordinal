{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
module Data.Ordinal.Kleene.Internal where 

import Prelude hiding (map)
import Data.Functor.Identity (Identity(..))

import Data.Ordinal.Zero
import Data.Ordinal.Lens

-- | Kleene star as applied to a transformer
--
-- t* b ~ b | t b | t (t b) | t (t (t b)) | ....
--
-- canonical form uses as few t layers as possible
-- e.g. @Pure a@ rather than @Wrap (Wrap (Lifted (Lifted a)))@
data Kleene t b where
  Pure :: Derived b => b -> Kleene t b
  Wrap :: Derived b => Kleene t (t b) -> Kleene t b

-- | proof that a ~ t^k b for some k
data Context a t b where
  Refl :: Derived a => Context a t a
  Impl :: Derived (t a) => Context a t b -> Context (t a) t b

-- both Kleene and Context act as lists of dictionaries for
-- Derived a, but in opposite order
data View f t b where
  View :: Context a t b -> f a -> View f t b
  
-- | properties we're interested in preserving
type Derived a = (HasZero a, Ord a, Num a)

-- | express each value in the container in as few t layers as possible.
--
-- for example, if
--
--    pf :: Context (Expansion (Expansion (Expansion b))) Expansion b
--    pf = Impl (Impl (Impl Refl))
--
--    fa :: [Expansion (Expansion (Expansion b))]
--    fa = [Lifted α, Lifted (Lifted β), Lifted (Lifted (Lifted γ)), Zero]
--
-- then @(pf', ks) = closeView (View pf fa)@ means
--
--    pf' :: Context b Expansion b
--    pf' = Refl
--
--    ks :: [Kleene Expansion b]
--    ks = [Wrap (Wrap (Pure α))), Wrap (Pure β)), Pure γ, Pure Zero]
--
-- note how we're careful to avoid constructing any values that look like @... Wrap (Pure (Lifted ...@
--
closeView :: forall f t b. (Traversable f, LensBase t) => View f t b -> (Context b t b, f (Kleene t b))
closeView = \(View pf (fmap (simplify pf) -> fk)) -> (refl pf fk, fk) where

  -- remove any t layers that don't add information to a value
  simplify :: forall x y. Context x t y -> x -> Kleene t y
  simplify (Impl pf@(context -> QED)) (viewBase -> (x, Zero)) = simplify pf x
  simplify pf@(context -> QED) x = wrap pf (Pure x)

  wrap :: forall x y. Context x t y -> Kleene t x -> Kleene t y
  wrap (Impl pf@(context -> QED)) k = wrap pf (Wrap k)
  wrap Refl k = k

  -- only generate the proof ourselves if the container is empty,
  -- since any value will contain the proof
  refl :: forall a. Context a t b -> f (Kleene t b) -> Context b t b
  refl pf = foldr (\(kleene -> QED) _ -> Refl) (unroll pf)

  unroll :: forall a. Context a t b -> Context b t b
  unroll Refl = Refl
  unroll (Impl pf) = unroll pf

-- | find the smallest k s.t. every value in the container can be expressed as @a ~ t^k b@
--
-- for example, if
--
--    ks :: [Kleene Expansion b]
--    ks = [Wrap (Wrap (Wrap (Pure α))), Wrap (Wrap (Pure β)), Wrap (Pure γ), Pure Zero]
--
-- then @View pf fa = openView Refl ks@ means
--
--    pf :: Context (Expansion (Expansion b)) Expansion b
--    pf = Impl (Impl (Impl Refl))
--
--    fa :: [Expansion (Expansion (Expansion b))]
--    fa = [α, (Lifted β), (Lifted (Lifted γ)), Zero]
-- 
openView :: forall f t b. (Traversable f, LensBase t) => Context b t b -> f (Kleene t b) -> View f t b
openView = \pf fk -> unify pf fk where
  -- make the container's values all have the same number of t layers
  -- (this is the inverse of "simplifying")
  unify :: forall a'. Context a' t b -> f (Kleene t a') -> View f t b
  unify pf@(context -> QED) fk = case traverse unpure fk of
    Left QED -> unify (Impl pf) (unwrap <$> fk)
    Right fa -> View pf fa 

  unpure :: forall a. Kleene t a -> Either (HasDerived (t a)) a
  unpure (Pure a) = Right a
  unpure (Wrap k) = Left (kleene k)

  unwrap :: forall a. Derived (t a) => Kleene t a -> Kleene t (t a)
  unwrap (Wrap k) = k
  unwrap (Pure a) = Pure (fromBase a)

data Pair a = Pair a a deriving (Functor, Foldable, Traversable)

-- | helper function to extend operations on Derived types to t* b
apply :: LensBase t => (forall a. Derived a => a -> a -> a) -> Kleene t b -> Kleene t b -> Kleene t b
apply (#) j k@(kleene -> QED) = case openView Refl (Pair j k) of
  View pf@(context -> QED) (Pair a b) -> toKleene pf (a # b)

map :: LensBase t => (forall a. Derived a => a -> a) -> Kleene t b -> Kleene t b
map f k = k `fromKleene` \pf@(context -> QED) a -> toKleene pf (f a)

-- | smart constructor
toKleene :: LensBase t => Context a t b -> a -> Kleene t b
toKleene pf a = case closeView (View pf (Identity a)) of (_, Identity k) -> k

-- | catamorphism
fromKleene :: LensBase t => Kleene t b -> (forall a. Context a t b -> a -> x) -> x
fromKleene k@(kleene -> QED) f = case openView Refl (Identity k) of
  View pf (Identity a) -> f pf a

instance Derived b => HasZero (Kleene t b) where
  isZero (Pure Zero) = True
  isZero _ = False
  zero = Pure Zero

instance LensBase t => Eq (Kleene t b) where
  j == k = case j `compare` k of EQ -> True ; _ -> False

instance LensBase t => Ord (Kleene t b) where
  compare j k@(kleene -> QED) = case openView Refl (Pair j k) of
    View (context -> QED) (Pair a b) -> compare a b

instance (Derived b, LensBase t) => Num (Kleene t b) where
  (+) = apply (+)
  (*) = apply (*)
  (-) = apply (-)
  negate = map negate
  abs = map abs
  signum = map signum
  fromInteger = toKleene Refl . fromInteger
  
-- | proof that Derived holds for a
data HasDerived a where
  QED :: Derived a => HasDerived a

-- | peek at the head of the Context's list of Derived dictionaries
context :: Context a t b -> HasDerived a
context Refl = QED
context (Impl _) = QED

-- | peek at the head of the Kleene's list of Derived dictionaries
kleene :: Kleene t b -> HasDerived b
kleene (Pure _) = QED
kleene (Wrap _) = QED

implies :: HasDerived b -> (Derived b => x) -> x
implies QED = id

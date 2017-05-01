{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Ordinal.Kleene.Internal where 

import Prelude hiding (map, (^))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))

import Data.Ordinal.Positive.Internal hiding (apply, map)
import Data.Ordinal.Zero
import Data.Ordinal.Lens
import Data.Ordinal.LPred
import Data.Ordinal.Pow
import Data.Ordinal.Minus

-- | Kleene star as applied to a transformer
--
-- t* b ~ b | t b | t (t b) | t (t (t b)) | ....
--
-- canonical form uses as few t layers as possible
-- e.g. @Point a@ rather than @Lower (Lower (Lifted (Lifted a)))@
data Kleene t b where
  Point :: Derived b => b -> Kleene t b
  Lower :: Derived b => Kleene t (t b) -> Kleene t b

-- | proof that a ~ t^k b for some k
data Context a t b where
  Reflexive :: Derived a => Context a t a
  Inductive :: Derived (t a) => Context a t b -> Context (t a) t b

-- both Kleene and Context act as lists of dictionaries for
-- Derived a, but in opposite order
--
-- View is like a single-state zipper, where we 
-- can strip Lower constructors from a container of Kleene
-- values, using fromBase on Point values until they're all Point
data View f t b where
  View :: Context a t b -> f a -> View f t b
  
-- | properties we're interested in preserving
type Derived a = (HasZero a, Ord a, Num a, LPred a, Pow a, Minus a)

-- | express each value in the container in as few t layers as possible.
--
-- for example, if
--
--    pf :: Context (Expansion (Expansion (Expansion b))) Expansion b
--    pf = Inductive (Inductive (Inductive Reflexive))
--
--    fa :: [Expansion (Expansion (Expansion b))]
--    fa = [Lifted α, Lifted (Lifted β), Lifted (Lifted (Lifted γ)), Zero]
--
-- then @(pf', ks) = closeView (View pf fa)@ means
--
--    pf' :: Context b Expansion b
--    pf' = Reflexive
--
--    ks :: [Kleene Expansion b]
--    ks = [Lower (Lower (Point α))), Lower (Point β)), Point γ, Point Zero]
--
-- note how we're careful to avoid constructing any values that look like @... Lower (Point (Lifted ...@
--
closeView :: forall f t b. (Traversable f, LensBase t) => View f t b -> (Context b t b, f (Kleene t b))
closeView = \(View pf (fmap (simplify pf) -> fk)) -> (reflexive pf fk, fk) where

  -- remove any t layers that don't add information to a value
  simplify :: forall x. Context x t b -> x -> Kleene t b
  simplify (Inductive pf@(context -> QED)) (viewBase -> (x, Zero)) = simplify pf x
  simplify pf@(context -> QED) x = lower pf (Point x)

  -- think of this like @Kleene t (t (t ... (t b))) -> Kleene t b@
  lower :: forall x. Context x t b -> Kleene t x -> Kleene t b
  lower (Inductive pf@(context -> QED)) k = lower pf (Lower k)
  lower Reflexive k = k

  -- only generate the proof ourselves if the container is empty,
  -- since any value will contain the proof
  reflexive :: forall a. Context a t b -> f (Kleene t b) -> Context b t b
  reflexive pf = foldr (\(kleene -> QED) _ -> Reflexive) (unroll pf)

  unroll :: forall a. Context a t b -> Context b t b
  unroll Reflexive = Reflexive
  unroll (Inductive pf) = unroll pf

-- | find the smallest k s.t. every value in the container can be expressed as @a ~ t^k b@
--
-- for example, if
--
--    ks :: [Kleene Expansion b]
--    ks = [Lower (Lower (Lower (Point α))), Lower (Lower (Point β)), Lower (Point γ), Point Zero]
--
-- then @View pf fa = openView Reflexive ks@ means
--
--    pf :: Context (Expansion (Expansion b)) Expansion b
--    pf = Inductive (Inductive (Inductive Reflexive))
--
--    fa :: [Expansion (Expansion (Expansion b))]
--    fa = [α, (Lifted β), (Lifted (Lifted γ)), Zero]
-- 
openView :: forall f t b. (Traversable f, LensBase t) => Context b t b -> f (Kleene t b) -> View f t b
openView = \pf fk -> unify pf fk where
  -- make the container's values all have the same number of t layers
  -- (this is the inverse of "simplifying")
  unify :: forall a'. Context a' t b -> f (Kleene t a') -> View f t b
  unify pf@(context -> QED) fk = case traverse getPoint fk of
    -- some value didn't use the @Point@ constructor
    Left QED -> unify (Inductive pf) (lift <$> fk)
    Right fa -> View pf fa 

  getPoint :: forall a. Kleene t a -> Either (HasDerived (t a)) a
  getPoint (Point a) = Right a
  getPoint (Lower k) = Left (kleene k)

  lift :: forall a. Derived (t a) => Kleene t a -> Kleene t (t a)
  lift (Lower k) = k
  lift (Point a) = Point (fromBase a)

data Pair a = Pair a a deriving (Functor, Foldable, Traversable)

applyF :: (Functor f, LensBase t) => (forall a. Derived a => a -> a -> f a) -> Kleene t b -> Kleene t b -> f (Kleene t b)
applyF op j k@(kleene -> QED) = case openView Reflexive (Pair j k) of
  View pf@(context -> QED) (Pair a b) -> toKleene pf <$> (a `op` b)

-- | a delayed composition operator, useful with @applyF@
(#) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(#) f g a b = f $ g a b

-- | helper function to extend operations on Derived types to t* b
apply :: LensBase t => (forall a. Derived a => a -> a -> a) -> Kleene t b -> Kleene t b -> Kleene t b
apply op = runIdentity # applyF (Identity # op)

map :: LensBase t => (forall a. Derived a => a -> a) -> Kleene t b -> Kleene t b
map f k = k `fromKleene` \pf@(context -> QED) a -> toKleene pf (f a)

-- | smart constructor
toKleene :: LensBase t => Context a t b -> a -> Kleene t b
toKleene pf a = case closeView (View pf (Identity a)) of (_, Identity k) -> k

-- | catamorphism
fromKleene :: LensBase t => Kleene t b -> (forall a. Context a t b -> a -> x) -> x
fromKleene k@(kleene -> QED) f = case openView Reflexive (Identity k) of
  View pf (Identity a) -> f pf a

instance forall t. LensBase t => LensBase (Kleene t) where
  lensBase :: forall f b. Functor f => (b -> f b) -> Kleene t b -> f (Kleene t b)
  lensBase f k = k `fromKleene` \pf a -> toKleene pf <$> loop pf a where
    loop :: Context a t b -> a -> f a
    loop (Inductive pf@(context -> QED)) = lensBase $! loop pf
    loop Reflexive = f

instance Derived b => HasZero (Kleene t b) where
  isZero (Point Zero) = True
  isZero _ = False
  zero = Point Zero

instance LensBase t => Eq (Kleene t b) where
  j == k = case j `compare` k of EQ -> True ; _ -> False

instance LensBase t => Ord (Kleene t b) where
  compare = getConst # applyF (Const # compare)

instance (Derived b, LensBase t) => Num (Kleene t b) where
  (+) = apply (+)
  (*) = apply (*)
  (-) = apply (-)
  negate = map negate
  abs = map abs
  signum = map signum
  fromInteger = toKleene Reflexive . fromInteger

instance LensBase t => LPred (Kleene t b) where
  lpred = map (lpred . Positive) . getPositive

instance LensBase t => Pow (Kleene t b) where
  (^) = apply (^)

instance LensBase t => Minus (Kleene t b) where
  minus = applyF minus
  
-- | proof that Derived holds for a
data HasDerived a where
  QED :: Derived a => HasDerived a

-- | peek at the head of the Context's list of Derived dictionaries
context :: Context a t b -> HasDerived a
context Reflexive = QED
context (Inductive _) = QED

-- | peek at the head of the Kleene's list of Derived dictionaries
kleene :: Kleene t b -> HasDerived b
kleene (Point _) = QED
kleene (Lower _) = QED

implies :: HasDerived b -> (Derived b => x) -> x
implies QED = id

-- | compiler check that we've successfully derived instances for all the
-- Derived classes for Kleene, so we can iterate it
derivesDerived :: (LensBase t, Derived b) => HasDerived (Kleene t b)
derivesDerived = QED

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Ordinal.Kleene.Internal where 

import GHC.Exts (Constraint)
import Data.Ordinal.Zero
import Data.Ordinal.Lens

-- think of Kleene and IsKleene it as lists of dictionaries in opposite order

data Kleene t b where
  Pure :: HasZero b => b -> Kleene t b
  Wrap :: HasZero b => Kleene t (t b) -> Kleene t b

data IsKleene a t b where
  Refl :: HasZero a => IsKleene a t a
  Impl :: HasZero (t a) => IsKleene a t b -> IsKleene (t a) t b

data ViewKleene t b where
  ViewKleene :: IsKleene a t b -> a -> ViewKleene t b

instance HasZero b => HasZero (Kleene t b) where
  isZero (Pure Zero) = True
  isZero _ = False
  zero = Pure Zero

instance HasZero b => HasZero (ViewKleene t b) where
  isZero (ViewKleene Refl Zero) = True
  isZero _ = False
  zero = ViewKleene Refl Zero

-- | smart constructor
toViewKleene :: forall a t b. (ToKleene a t b, LensBase t) => a -> ViewKleene t b
toViewKleene = simplify isKleene where
  simplify :: forall x. IsKleene x t b -> x -> ViewKleene t b
  simplify (Impl pf@(starHasZero -> QED)) (viewBase -> (x, Zero)) = simplify pf x
  simplify pf x = ViewKleene pf x

-- | smart constructor
toKleene :: forall a t b. (ToKleene a t b, LensBase t) => a -> Kleene t b
toKleene = fromViewKleene . toViewKleene

-- fromViewKleene & fromKleene traverse the list of dictionaries and reverse it

fromViewKleene :: forall t b. ViewKleene t b -> Kleene t b
fromViewKleene = \(ViewKleene pf a) -> starHasZero pf `implies` wrap pf (Pure a) where
  wrap :: forall x y. IsKleene x t y -> Kleene t x -> Kleene t y
  wrap (Impl pf) k = starHasZero pf `implies` wrap pf (Wrap k)
  wrap Refl k = k

fromKleene :: forall t b. Kleene t b -> ViewKleene t b
fromKleene = \k -> baseHasZero k `implies` unwrap k Refl where
  unwrap :: forall x y. Kleene t x -> IsKleene x t y -> ViewKleene t y
  unwrap (Wrap k) pf = baseHasZero k `implies` unwrap k (Impl pf)
  unwrap (Pure b) pf = ViewKleene pf b

starHasZero :: IsKleene a t b -> Claim '[HasZero] a
starHasZero (Impl _) = QED
starHasZero Refl = QED

baseHasZero :: Kleene t b -> Claim '[HasZero] b
baseHasZero (Wrap _) = QED
baseHasZero (Pure _) = QED

class ToKleene a t b where
  isKleene :: IsKleene a t b

instance HasZero a => ToKleene a t a where
  isKleene = Refl

instance (HasZero (t a), ToKleene a t b) => ToKleene (t a) t b where
  isKleene = Impl isKleene

data Claim (cs :: [* -> Constraint]) (a :: *) where
  QED :: All cs a => Claim cs a

type family All (cs :: [* -> Constraint]) (a :: *) :: Constraint where
  All '[] a = ()
  All (c ': cs) a = (c a, All cs a)

implies :: Claim cs a -> (All cs a => x) -> x
QED `implies` x = x
{-
class Preserves (cs :: [* -> Constraint]) (t :: * -> *) where
  preserves :: All cs a => Claim cs (t a)

-}


{-
toKleene' :: forall a b t. 
  (ToKleene a t b, HasZero b, HasZero a, LensBase t) => 
  a -> Kleene t b
toKleene' = test where
  test :: a -> Kleene t b
  test Zero = Zero
  test a = loop (toKleene a) a

  loop :: forall x. Kleene t b -> x -> Kleene t b
  loop (Wrap _) (viewBase -> (a, Zero)) = loop (toKleene a) a
  loop k _ = k
-}



{-
data Kleene cs t b where
  Kleene :: a `IsSome` t `Of` b `With` cs -> a -> Kleene cs t b

instance HasZero b => HasZero (Kleene '[HasZero] t b) where
  zero = Kleene Reflexive zero
  isZero (Kleene Reflexive b) = isZero b
  isZero _ = False

class Derives cs t where
  derives :: Satisfied cs a -> Satisfied cs (t a)


-- | smart constructor
toKleene :: forall a b t. (HasZero b, LensBase t, Derives '[HasZero] t) => a `IsSome` t `Of` b -> a -> Kleene t b
toKleene = \pf -> hasZero pf `implies` test pf where
  hasZero :: forall x. x `IsSome` t `Of` b -> Satisfied '[HasZero] x
  hasZero = induction derives

  test :: HasZero a => a `IsSome` t `Of` b -> a -> Kleene t b
  test _ Zero = Zero
  test pf a = loop pf a

  -- a is nonzero
  loop :: forall x. HasZero x => x `IsSome` t `Of` b -> x -> Kleene t b
  loop Reflexive a = Kleene Reflexive a
  loop (Inductive pf) ta = hasZero pf `implies` case viewBase ta of
    (a, Zero) -> loop pf a
    _         -> Kleene (Inductive pf) ta

-}

{-
--import GHC.Exts (Constraint)
import Data.Ordinal.Finite
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
-- import Data.Ordinal.Positive.Internal
-- import Data.Ordinal.Expansion
-- import Data.Ordinal.Minus
-- import Data.Ordinal.LPred


type x `IsSomeExpansionOf` a = x `IsSome` Expansion `Of` a

data AnyExpansionF f a where
  AnyExpansionF :: x `IsSomeExpansionOf` a -> f x -> AnyExpansionF f a

type AnyExpansion = AnyExpansionF Identity

{-
-- | smart constructor
toAnyExpansion :: (Num a, a `HasExpansion` b) => b -> AnyExpansion a
toAnyExpansion = lower hasExpansion
-}

lower :: Num a => b `IsSomeExpansionOf` a -> b -> AnyExpansion a
lower (Inductive pf) (Lifted a) = lower pf a
lower (Inductive _) Zero = AnyExpansionF Reflexive $ Identity 0
lower pf a = AnyExpansionF pf $ Identity a

unify :: (Eq a, Num a, Functor f, Functor g) => 
  AnyExpansionF f a -> AnyExpansionF g a -> AnyExpansionF (Product f g) a
AnyExpansionF Reflexive fa `unify` AnyExpansionF ay gy = AnyExpansionF ay $ Pair (lift ay <$> fa) gy
AnyExpansionF ax fx `unify` AnyExpansionF Reflexive ga = AnyExpansionF ax $ Pair fx (lift ax <$> ga)
AnyExpansionF ax@(Inductive _) fx `unify` AnyExpansionF ay@(Inductive _) gy = case total ax ay of
  Left xy -> AnyExpansionF ay $ Pair (liftExpansion xy <$> fx) gy
  Right yx -> AnyExpansionF ax $ Pair fx (liftExpansion yx <$> gy)

type AnyExpansionPair = AnyExpansionF (Product Identity Identity)

pattern AnyExpansion :: x `IsSomeExpansionOf` a -> x -> AnyExpansion a
pattern AnyExpansion pf a = AnyExpansionF pf (Identity a)

pattern AnyExpansionPair :: x `IsSomeExpansionOf` a -> x -> x -> AnyExpansionPair a
pattern AnyExpansionPair pf a b = AnyExpansionF pf (Pair (Identity a) (Identity b))

instance (Num a, Ord a) => Ord (AnyExpansion a) where
  x `compare` y = 
    case unify x y of 
      AnyExpansionF pz (Pair (Identity xz) (Identity yz)) -> 
        case isOrd pz of 
          Satisfied -> compare xz yz

instance (Num a, Ord a) => Eq (AnyExpansion a) where
  x == y = case compare x y of EQ -> True ; _ -> False 

instance (Ord a, Num a, Minus a) => Num (AnyExpansion a) where
  x + y =
    case unify x y of 
      AnyExpansionF pz (Pair (Identity xz) (Identity yz)) -> 
        case isNum pz of 
          Satisfied -> AnyExpansion pz $ xz + yz
  x * y =
    case unify x y of 
      AnyExpansionF pz (Pair (Identity xz) (Identity yz)) -> 
        case isNum pz of 
          Satisfied -> AnyExpansion pz $ xz * yz
  x - y = case x `minus` y of
    RightDiff _ -> error "subtraction is not closed on AnyExpansion numbers"
    NoDiff      -> AnyExpansion Reflexive 0
    LeftDiff γ  -> γ

  abs = id
  signum (AnyExpansionF ax (Identity x)) = case isNum ax of
    Satisfied -> AnyExpansion Reflexive (if x == 0 then 0 else 1)
  negate = error "negation is not defined for AnyExpansion numbers"
  fromInteger = AnyExpansion Reflexive . fromInteger

isMinus :: (Ord a, Minus a) => b `IsSomeExpansionOf` a -> Satisfied '[Ord, Minus] b
isMinus = induction $ \Satisfied -> Satisfied

instance (Num a, Ord a, Minus a) => Minus (AnyExpansion a) where
  x `minus` y =
    case unify x y of 
      AnyExpansionF pz (Pair (Identity xz) (Identity yz)) -> 
        case isMinus pz of
          Satisfied -> 
            case minus xz yz of
              RightDiff c -> RightDiff $ lower pz c
              NoDiff      -> NoDiff
              LeftDiff c  -> LeftDiff $ lower pz c

instance LPred a => LPred (AnyExpansion a) where
  lpred (Positive (AnyExpansionF Reflexive (Identity a))) =
    AnyExpansionF Reflexive . Identity . lpred $ Positive a
  lpred (Positive a) = a

{-
instance LensFinite a => LensFinite (AnyExpansion a) where     
  lensFinite f 
  -}

-- instance Pow a => LensFinite (Pow a) where     
-}

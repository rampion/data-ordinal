{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- needed to define MaxEpsilon in terms of MSucc
{-# LANGUAGE BangPatterns #-}
module Data.Ordinal.Epsilon.Internal where
import Unsafe.Coerce (unsafeCoerce)

import Data.Ordinal.Finite
import Data.Ordinal.Expansion.Internal
import Data.Ordinal.Kleene.Internal
import Data.Ordinal.Lens

-- | unary encoded naturals
data Nat = NZero | NSucc Nat

-- | singletons for naturals
data SNat (n :: Nat) where
  SZero :: SNat 'NZero
  SSucc :: SNat n -> SNat ('NSucc n)

fromSNat :: SNat n -> Int
fromSNat = loop 0 where
  loop :: Int -> SNat m -> Int
  loop !n (SSucc sn) = loop (1+n) sn
  loop !n SZero = n

asSNat :: (forall n. SNat n -> x) -> Int -> Maybe x
asSNat = test where
  test :: (forall n. SNat n -> x) -> Int -> Maybe x
  test f i | i >= 0     = Just (loop f i)
           | otherwise  = Nothing
  loop :: (forall n. SNat n -> x) -> Int -> x
  loop f 0 = f SZero
  loop f i = loop (f . SSucc) (i - 1)

-- | proof that m <= n, where 'Nothing means ∞
--
--   implemented as a unary encoding of n - m
--   for finite n, and as an axiom otherwise
data LTE (m :: Nat) (n :: Maybe Nat) where 
  LEqual :: LTE' m m
  LTrian :: LTE' m n -> LTE' m ('NSucc n)
  LInfin :: LTE m 'Nothing

type LTE' m n = LTE m ('Just n)

-- | the naturals are totally ordered
hasTotalOrder :: SNat m -> SNat n -> Either (LTE' m n) (LTE' n m)
hasTotalOrder (SSucc sm) (SSucc sn) = either (Left . bisucc) (Right . bisucc) `noop` hasTotalOrder sm sn
hasTotalOrder SZero sn = Left (toLTE sn)
hasTotalOrder sm SZero = Right (toLTE sm)

-- | m <= n => m + 1 <= n + 1
bisucc :: LTE' m n -> LTE' ('NSucc m) ('NSucc n)
bisucc = noop loop where
  loop :: LTE' x y -> LTE' ('NSucc x) ('NSucc y)
  loop (LTrian pf) = LTrian (loop pf)
  loop LEqual = LEqual

-- | 0 <= n
toLTE :: SNat n -> LTE' 'NZero n
toLTE (SSucc sn) = LTrian (toLTE sn)
toLTE SZero = LEqual

-- | use @noop f@ only if, for all @a@, @f a@ is built of the same
--   type constructors, simply with a relabled type.
--   
--   It makes an O(|a|) operation into an O(1) one.
--   Replacing @noop@ with @id@ shouldn't change the computed values, just the runtime
noop :: (a -> b) -> a -> b
noop = const unsafeCoerce

type family MSucc (n :: Maybe Nat) :: Maybe Nat where
  MSucc 'Nothing = 'Nothing
  MSucc ('Just n) = 'Just ('NSucc n)

-- | singletons for maybe
data SMaybe (f :: k -> *) (m :: Maybe k) where
  SNothing :: SMaybe f 'Nothing
  SJust :: f k -> SMaybe f ('Just k)

data HasEpsilonD a n = HasEpsilonD
  { -- | useful when deriving HasEpsilon (f a) from HasEpsilon a, as it lets us
    --   check the value of MaxEpsilon a
    maxEpsilon :: SMaybe SNat n
  , matchEpsilon :: forall x. a -> (forall m. LTE m n -> SNat m -> x) -> Maybe x
  , epsilon :: forall m. LTE m n -> SNat m -> a
  }

-- | (HasEpsilon a, MaxEpsilon a ~ n) => 
--    (HasEpsilon (Expansion a), MaxEpsilon (Expansion a) ~ MSucc n)
expansionD :: (Num a, Eq a) => HasEpsilonD a n -> HasEpsilonD (Expansion a) (MSucc n)
expansionD (HasEpsilonD { maxEpsilon = SNothing, ..}) = HasEpsilonD
  { maxEpsilon = SNothing
  , matchEpsilon = \case
      Lifted a  -> matchEpsilon a
      _         -> const Nothing
  , epsilon = \LInfin sn -> Lifted (epsilon LInfin sn)
  }
expansionD (HasEpsilonD { maxEpsilon = SJust sn, ..}) = HasEpsilonD
  { maxEpsilon = SJust sn'
  , matchEpsilon = \case
      Infinity  -> \f -> Just (f LEqual sn')
      Lifted a  -> \f -> matchEpsilon a $ \pf sm -> f (LTrian pf) sm
      _         -> const Nothing
  , epsilon = \case
      LTrian pf -> Lifted . epsilon pf
      LEqual -> \_sn' -> Infinity
  } where sn' = SSucc sn

-- | (HasEpsilon a, MaxEpsilon a ~ n) =>
--    (HasEpsilon (Kleene Expansion a), MaxEpsilon (Kleene Expansion a) ~ 'Nothing)
kleeneD :: forall a n. (Derived a, LensFinite a) => HasEpsilonD a n -> HasEpsilonD (Kleene Expansion a) 'Nothing
kleeneD (HasEpsilonD { maxEpsilon = SNothing, .. }) = HasEpsilonD 
  { maxEpsilon = SNothing
  , matchEpsilon = \case
      Point eea -> matchEpsilon eea
      _         -> const Nothing
  , epsilon = \pf -> Point . epsilon pf
  }
kleeneD (HasEpsilonD { maxEpsilon = SJust sn, .. }) = HasEpsilonD
  { maxEpsilon = SNothing
  , matchEpsilon = 
    let loop :: (Eq b, Num b) => Kleene Expansion (Expansion b) -> (forall m. SNat m -> x) -> Maybe x
        loop (Lower k) = \f -> loop k $ f . SSucc
        loop (Point Infinity) = \f -> Just (f (SSucc sn))
        loop _ = const Nothing
    in \case
      Point eea -> \f -> matchEpsilon eea (\_pf -> f LInfin)
      Lower k   -> \f -> loop k (f LInfin)
  , epsilon = 
      let loop :: (Derived b, LensFinite b) => LTE' n' m -> Kleene Expansion (Expansion b)
          loop (LTrian pf) = Lower (loop pf)
          loop LEqual = Point Infinity
          test sm LEqual = Point (epsilon LEqual sm)
          test _ (LTrian pf) = Lower (loop pf)
      in \LInfin sm -> either (\pf -> Point (epsilon pf sm)) (test sm) $ hasTotalOrder sm sn
  }

class HasEpsilon a where
  type MaxEpsilon a :: Maybe Nat
  hasEpsilonD :: HasEpsilonD a (MaxEpsilon a)

instance HasEpsilon (Expansion (Expansion Finite)) where
  type MaxEpsilon (Expansion (Expansion Finite)) = 'Just 'NZero
  hasEpsilonD = HasEpsilonD
    { maxEpsilon = SJust SZero
    , matchEpsilon = \case
        Infinity  -> \f -> Just (f LEqual SZero)
        _         -> const Nothing
    , epsilon = \LEqual SZero -> Infinity
    } 

-- split (HasEpsilon a => HasEpsilon (Expansion a)) over three instances so it 
-- doesn't overlap with HasEpsilon (Expansion (Expansion Finite))
instance (Derived a, LensFinite a, HasEpsilon (Expansion (Expansion a))) =>
    HasEpsilon (Expansion (Expansion (Expansion a))) where
  type MaxEpsilon (Expansion (Expansion (Expansion a))) = MSucc (MaxEpsilon (Expansion (Expansion a)))
  hasEpsilonD = expansionD hasEpsilonD

instance (Derived b, LensBase t, HasEpsilon (Kleene t b)) => HasEpsilon (Expansion (Kleene t b)) where
  type MaxEpsilon (Expansion (Kleene t b)) = MSucc (MaxEpsilon (Kleene t b))
  hasEpsilonD = expansionD hasEpsilonD

instance (Derived b, LensBase t, HasEpsilon (Expansion (Kleene t b))) =>
    HasEpsilon (Expansion (Expansion (Kleene t b))) where
  type MaxEpsilon (Expansion (Expansion (Kleene t b))) = MSucc (MaxEpsilon (Expansion (Kleene t b)))
  hasEpsilonD = expansionD hasEpsilonD

-- derived from HasEpsilon (Kleene Expansion (Expansion (Expansion Finite)))
instance HasEpsilon (Kleene Expansion Finite) where
  type MaxEpsilon (Kleene Expansion Finite) = 'Nothing
  hasEpsilonD = HasEpsilonD 
    { maxEpsilon = SNothing
    , matchEpsilon = \case 
        Lower (Lower k) -> matchEpsilon k
        _               -> const Nothing
    , epsilon = \LInfin -> Lower . Lower . epsilon LInfin
    } where HasEpsilonD { .. } = hasEpsilonD

-- derived from HasEpsilon (Kleene Expansion (Expansion (Expansion Finite)))
instance HasEpsilon (Kleene Expansion (Expansion Finite)) where
  type MaxEpsilon (Kleene Expansion (Expansion Finite)) = 'Nothing
  hasEpsilonD = HasEpsilonD
    { maxEpsilon = SNothing
    , matchEpsilon = \case 
        Lower k -> matchEpsilon k
        _       -> const Nothing
    , epsilon = \LInfin -> Lower . epsilon LInfin
    } where HasEpsilonD { .. } = hasEpsilonD

-- split (HasEpsilon a => HasEpsilon (Kleene Expansion a)) over three instances
-- so it doesn't overlap with HasEpsilon (Kleene Expansion Finite)
instance (Derived a, LensFinite a, HasEpsilon (Expansion (Expansion a))) =>
    HasEpsilon (Kleene Expansion (Expansion (Expansion a))) where
  type MaxEpsilon (Kleene Expansion (Expansion (Expansion a))) = 'Nothing
  hasEpsilonD = kleeneD hasEpsilonD

instance (Derived b, LensFinite b, LensBase t, HasEpsilon (Expansion (Kleene t b))) =>
    HasEpsilon (Kleene Expansion (Expansion (Kleene t b))) where
  type MaxEpsilon (Kleene Expansion (Expansion (Kleene t b))) = 'Nothing
  hasEpsilonD = kleeneD hasEpsilonD

instance (Derived b, LensFinite b, LensBase t, HasEpsilon (Kleene t b)) =>
    HasEpsilon (Kleene Expansion (Kleene t b)) where
  type MaxEpsilon (Kleene Expansion (Kleene t b)) = 'Nothing
  hasEpsilonD = kleeneD hasEpsilonD

instance (LensBase t, Derived b, MaxEpsilon (Kleene t b) ~ 'Nothing, HasEpsilon (Kleene t b)) =>
    HasEpsilon (Kleene (Kleene t) b) where
  type MaxEpsilon (Kleene (Kleene t) b) = 'Nothing
  hasEpsilonD = HasEpsilonD
    { maxEpsilon = SNothing
    , matchEpsilon = \case
        Lower (Point k) -> matchEpsilon k
        _               -> const Nothing
    , epsilon = \pf -> Lower . Point . epsilon pf
    } where HasEpsilonD { .. } = hasEpsilonD

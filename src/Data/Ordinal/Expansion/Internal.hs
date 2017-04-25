{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Data.Ordinal.Expansion.Internal where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Prelude hiding ((^))

import Data.Ordinal.Finite
import Data.Ordinal.Positive.Internal
import Data.Ordinal.NonNegative.Internal
import Data.Ordinal.Minus
import Data.Ordinal.LPred
import Data.Ordinal.Pow

-- | The closure of @a U {∞}@ under addition, multiplication, and
-- exponentiation.
--
-- Notation:
--   *  α,β,γ :: Expansion a
--   *  a,b,c :: Positive a | NonNegative a | a
--   *  x,y,z :: Finite
--   *  p,q,v :: (Expansion a, Positive a)
--   * _s, _t :: [_]
--
-- Invariant:
--    > α = Ordinal [(α_k,a_k),...,(α_1,a_1),(α_0,a_0)]
--    >   = ∞ ^ α_k * a_k + ... + ∞ ^ α_1 * a_1 + ∞ ^ α_0 * a_0
-- s.t. 
--    > α_k > ... > α_1 * a_1 > α_0
--
-- This encoding of α is unique and is referred to as its
-- "base δ expansion". In the case when @a ~ Finite@, it
-- is also called the "Cantor Normal Form" (CNF)
newtype Expansion a = Expansion { getExpansion :: [(Expansion a, Positive a)] }
  deriving (Eq, Ord)

pattern Zero :: Expansion a
pattern Zero = Expansion []

pattern One :: (Eq a, Num a) => Expansion a
pattern One = Lifted 1

pattern Lifted :: a -> Expansion a
pattern Lifted a = Expansion [(Zero, Positive a)]

pattern Infinity :: (Eq a, Num a) => Expansion a
pattern Infinity = Expansion [(One, Positive 1)]

pattern Omega :: Expansion Finite
pattern Omega = Infinity

ω :: Expansion Finite
ω = Omega

pattern EpsilonNaught :: Expansion (Expansion Finite)
pattern EpsilonNaught = Infinity

ε_0 :: Expansion (Expansion Finite)
ε_0 = EpsilonNaught

-- ε_1 = Infinity :: Expansion (Expansion (Expansion Finite))

-- | A lens to access the coefficient of @∞ ^ 0@ in the base δ expansion
-- encoding.
lensNonNegative :: (Functor f, Eq a, Num a) => 
  (NonNegative a -> f (NonNegative a)) -> Expansion a -> f (Expansion a)
lensNonNegative f = fmap Expansion . foldr go (use 0) . getExpansion where
  use a_0 = f (NonNegative a_0) <&> \case
    NonNegative 0   -> []
    NonNegative b_0 -> [(Zero, Positive b_0)]
  (<&>) = flip fmap
  go (Zero, Positive a_0) = const $ use a_0
  go p = fmap (p:)

fromNonNegative :: (Eq a, Num a) => NonNegative a -> Expansion a
fromNonNegative a = runIdentity $ lensNonNegative (\_ -> Identity a) Zero

-- | Remove the @∞ ^ 0@ term in the base δ expansion encoding
--
-- Invariant: 
--    > viewNonNegative α = (a, α') =>
--    >   α' + fromNonNegative a = α AND
--    >   viewNonNegative α' = (0, α')
viewNonNegative :: (Eq a, Num a) => Expansion a -> (NonNegative a, Expansion a)
viewNonNegative = lensNonNegative (,NonNegative 0)

toExpansion :: (Ord a, Num a) => a -> Maybe (Expansion a)
toExpansion = fmap fromNonNegative . toNonNegative

-- | leading exponent of the base δ expansion of an ordinal (if any)
degree :: Expansion a -> Maybe (Expansion a)
degree (Expansion []) = Nothing
degree (Expansion ((α_k,_):_)) = Just α_k

-- | Incomplete: Expansion is only a near-semiring
--    * @(-)@ is partial
--    * @negate@ is an error
--    * @fromInteger@ is partial
instance (Ord a, Num a, Minus a) => Num (Expansion a) where
  -- Without loss of generality, let
  --    > α = α_gt + ∞ ^ γ * a + α_lt
  --    > β = ∞ ^ γ * b + β_lt
  -- Where
  --    * all the exponents in α_gt are greater than γ, 
  --    * all the exponents in α_lt and β_lt are less than γ.
  --    * α_gt and a may be 0
  -- Then
  --    > α + β = α_gt + ∞ ^ γ * (a + b) + β_lt
  α + Expansion [] = α
  Expansion ps + Expansion qs@((β_k,b_k):qt) = 
      Expansion . fromMaybe qs $ foldr go Nothing ps where
    -- Given β normalizes to ∞ ^ β_k * b_k + β'
    go p@(α_i,a_i) Nothing = case compare α_i β_k of
      -- α_i < β_k => ∞ ^ α_i * a_i + β normalizes to β
      LT -> Nothing 
      -- α_i = β_k => ∞ ^ α_i * a_i + β normalizes to ∞ ^ α_i * (a_i + b_k) + β'
      EQ -> Just $ (α_i,a_i + b_k) : qt
      -- α_i > β_k => ∞ ^ α_i * a_i + β is in normal form
      GT -> Just $ p : qs
    -- a_i > a_i' >= β_k, so ∞ ^ α_i * a_i is a term in the normal form
    go p (Just vs) = Just (p:vs)

  -- multiplication is left-distributive
  -- α * (∞ ^ β_k * b_k + ... + ∞ ^ β_1 * b_1 + ∞ ^ β_0 * b_0)
  -- = (α * ∞ ^ β_k * b_k) + ... + (α * ∞ ^ β_1 * b_1) + (α * ∞ ^ β_0 * b_0)
  --
  -- this is self-normalizing as
  --  * α > 0, β_i > β_j => α * ∞ ^ β_i > α * ∞ ^ β_j
  --  * β_i > 0 => (α * ∞ ^ β_i * b_i) normalizes to ∞ ^ γ_i * b_i
  Expansion [] * _ = Zero
  Expansion ((α_k,a_k):pt) * Expansion qs = Expansion $ foldr go [] qs where
    -- α * b_0
    --  = (∞ ^ α_k * a_k + α') * b_0
    --  = (∞ ^ α_k * a_k + α') + (∞ ^ α_k * a_k + α') + ... + (∞ ^ α_k * a_k + α')
    --  = ∞ ^ α_k * a_k + (α' + ∞ ^ α_k * a_k) + (α' + ... + ∞ ^ α_k * a_k) + α'
    --  = ∞ ^ α_k * a_k + ∞ ^ α_k * a_k + ... + ∞ ^ α_k * a_k + α'
    --  = ∞ ^ α_k * a_k * b_0 + α'
    go (Zero, b_0) _ = (α_k, a_k * b_0) : pt
    -- α * ∞ ^ β_i * b_i
    --  = (∞ ^ α_k * a_k + α') * ∞ ^ β_i * b_i
    --  = (∞ ^ α_k * a_k + α') + (∞ ^ α_k * a_k + α') + ... 
    --  = ∞ ^ α_k * a_k + (α' + ∞ ^ α_k * a_k) + (α' + ... 
    --  = ∞ ^ α_k * a_k + ∞ ^ α_k * a_k + ... 
    --  = (∞ ^ α_k * a_k) * ∞ ^ β_i * b_i
    --  = ∞ ^ α_k * (a_k * ∞ ^ β_i) * b_i
    --  = ∞ ^ α_k * ∞ ^ β_i * b_i
    --  = ∞ ^ (α_k + β_i) * b_i
    go (β_i, b_i) vs = (α_k + β_i, b_i) : vs

  α - β = case α `minus` β of
    RightDiff _ -> error "subtraction is not closed on Expansion numbers"
    NoDiff      -> Zero
    LeftDiff γ  -> γ

  negate = error "negation is not defined for Expansion numbers"
  abs = id
  signum Zero = Zero
  signum _ = One
  fromInteger n = fromMaybe (error msg) . toExpansion $ fromInteger n  where
    msg = shows n " can not be converted to a Expansion number"

instance (Ord a, Minus a) => Minus (Expansion a) where
  Expansion [] `minus` Expansion [] = NoDiff
  Expansion [] `minus` β = RightDiff β
  α `minus` Expansion [] = LeftDiff α
  Expansion ps@((α_i, a_i):pt) `minus` Expansion qs@((β_j, b_j):qt) = case α_i `compare` β_j of
      LT -> RightDiff $ Expansion qs
      GT -> LeftDiff $ Expansion ps 
      EQ -> case a_i `minus` b_j of
        RightDiff c -> RightDiff . Expansion $ (β_j, c) : qt
        LeftDiff c  -> LeftDiff . Expansion $ (α_i, c) : pt
        NoDiff -> Expansion pt `minus` Expansion qt

instance LPred a => LPred (Expansion a) where
  lpred (Positive (Lifted a)) = NonNegative . Lifted . getNonNegative . lpred $ Positive a
  lpred (Positive α) = NonNegative α

instance (Eq a, Num a, LensFinite a) => LensFinite (Expansion a) where
  lensFinite f = lensNonNegative (fmap NonNegative . lensFinite f. getNonNegative)

instance (Ord a, Num a, LPred a, Minus a, Pow a, LensFinite a) => Pow (Expansion a) where
  _ ^ Expansion [] = One
  Expansion [] ^ _ = Zero

  α ^ One = α
  One ^ _ = One

  -- a ^ (Infinity * β' + b)
  --  = (a ^ Infinity) ^ β' * (a ^ b)
  --  = Infinity ^ β' * (a ^ b)
  Lifted a ^ β = Expansion [(Expansion qs', Positive $ a ^ b)] where
    -- β = (Expansion qs)@(∞ ^ β_k * b_k + ... + ∞ ^ β_1 * b_1) + b
    (NonNegative b, Expansion qs) = viewNonNegative β
    -- β' = ∞ ^ β_k' * b_k + ... + ∞ ^ β_1' * b_1
    --  where 1 + β_i' = β_i
    qs' = first (getNonNegative . lpred . Positive) <$> qs

  -- 
  Expansion ((α_k,a_k):pt) ^ β = Expansion $ loop Nothing (Positive 1) γs where
    γs = (α_k *) <$> lensFinite (\(NonNegative z) -> NonNegative <$> [z, z-1 .. 0]) β

    loop _ c [γ] = [(γ, c)]
    loop a_k' c (γ:γt) = (γ, fromMaybe a_k a_k') : foldr (go γt a_k' c) [] pt
    loop _ _ [] = error "invalid finite term"

    go γt a_k' _ (Zero, a_0) _ = loop (a_k' <|> Just (a_k * a_0)) a_0 γt
    go (γ':_) _ _ (α_i, a_i) vs = (γ' + α_i, a_i) : vs
    go _ _ _ (_, _) _ = error "invalid finite term"

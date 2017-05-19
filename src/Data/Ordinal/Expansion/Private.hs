{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Ordinal.Expansion.Private where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Maybe (fromMaybe)
import Prelude hiding ((^))

import Data.Ordinal.Positive.Internal
import Data.Ordinal.Minus
import Data.Ordinal.LPred
import Data.Ordinal.Pow
import Data.Ordinal.Zero
import Data.Ordinal.Lens

-- | The closure of @a U {∞}@ under addition, multiplication, and
-- exponentiation.
--
-- Notation:
--   *  α,β,γ :: Expansion a
--   *  a,b,c :: Positive a | a
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

instance HasZero (Expansion a) where
  isZero = null . getExpansion
  zero = Expansion []

pattern One :: (Eq a, Num a) => Expansion a
pattern One = Lifted 1

pattern Lifted :: a -> Expansion a
pattern Lifted a = Expansion [(Zero, Positive a)]

pattern Infinity :: (Eq a, Num a) => Expansion a
pattern Infinity = Expansion [(One, Positive 1)]

-- | leading exponent of the base δ expansion of an ordinal (if any)
degree :: Expansion a -> Maybe (Expansion a)
degree (Expansion []) = Nothing
degree (Expansion ((α_k,_):_)) = Just α_k

showExpansion :: (HasZero a, Minus a, Num a, Ord a) => String -> (Int -> a -> ShowS) -> Int -> Expansion a -> ShowS
showExpansion sym showLifted = \p -> showTerms p . getExpansion where
  showTerms _ []  = showString "0"
  showTerms p [q] = showTerm p q
  showTerms p qs  = showParen (p >= 6) $ 
    foldr1 (\x y -> x . showString " + " . y) $ showTerm 6 <$> qs

  showTerm p (Zero, Positive a) = showLifted p a
  showTerm _ (One, Positive 1) = showString sym
  showTerm p (One, Positive a) = showParen (p >= 7) $
    showString sym . showString " * " . showLifted 7 a
  showTerm p (Expansion qs, Positive 1) = showParen (p > 8) $ 
    showString sym . showString " ^ " . showTerms 8 qs
  showTerm p (Expansion qs, Positive a) = showParen (p >= 7) $ 
    showString sym . showString " ^ " . showTerms 8 qs . showString " * " . showLifted 7 a

instance LensBase Expansion where
  -- | A lens to access the coefficient of @∞ ^ 0@ in the base δ expansion
  -- encoding.
  lensBase f = fmap Expansion . foldr go (use Zero) . getExpansion where
    use a_0 = f a_0 <&> \case
      Zero  -> []
      b_0   -> [(Zero, Positive b_0)]
    (<&>) = flip fmap
    go (Zero, Positive a_0) = const $ use a_0
    go p = fmap (p:)

-- | Incomplete: Expansion is only a near-semiring
--    * @(-)@ is partial
--    * @negate@ is an error
--    * @fromInteger@ is partial
instance (Ord a, Num a, Minus a, HasZero a) => Num (Expansion a) where
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
    -- XXX: above should use LensFinite rather than LensBase as
    --
    --  (ε0 + 1) * ω = ε0 * ω /= ε0 * ω + ω

  α - β = case α `minus` β of
    RightDiff _ -> error "subtraction is not closed on Expansion numbers"
    NoDiff      -> Zero
    LeftDiff γ  -> γ

  negate = error "negation is not defined for Expansion numbers"
  abs = id
  signum Zero = Zero
  signum _ = One
  fromInteger n = fromBase $ fromInteger n  where

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
  lpred (Positive (Lifted a)) = Lifted . lpred $ Positive a
  lpred (Positive α) = α

instance (Ord a, Num a, LPred a, Minus a, Pow a, HasZero a, LensFinite a) => Pow (Expansion a) where
  _ ^ Expansion [] = One
  Expansion [] ^ _ = Zero

  α ^ One = α
  One ^ _ = One

  -- a ^ (Infinity * β' + b)
  --  = (a ^ Infinity) ^ β' * (a ^ b)
  --  = Infinity ^ β' * (a ^ b)
  Lifted a ^ β = Expansion [(Expansion qs', Positive $ a ^ b)] where
    -- β = (Expansion qs)@(∞ ^ β_k * b_k + ... + ∞ ^ β_1 * b_1) + b
    (b, Expansion qs) = viewBase β
    -- β' = ∞ ^ β_k' * b_k + ... + ∞ ^ β_1' * b_1
    --  where 1 + β_i' = β_i
    qs' = first (lpred . Positive) <$> qs

  -- 
  Expansion ((α_k,a_k):pt) ^ β = Expansion $ loop Nothing (Positive 1) γs where
    γs = (α_k *) <$> lensFinite (\z -> [z, z-1 .. 0]) β

    loop _ c [γ] = [(γ, c)]
    loop a_k' c (γ:γt) = (γ, fromMaybe a_k a_k') : foldr (go γt a_k' c) [] pt
    loop _ _ [] = error "invalid finite term"

    go γt a_k' _ (Zero, a_0) _ = loop (a_k' <|> Just (a_k * a_0)) a_0 γt
    go (γ':_) _ _ (α_i, a_i) vs = (γ' + α_i, a_i) : vs
    go _ _ _ (_, _) _ = error "invalid finite term"

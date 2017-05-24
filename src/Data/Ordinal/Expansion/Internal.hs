{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Ordinal.Expansion.Internal where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Data.Maybe (fromMaybe)
import Data.Typeable
import Prelude hiding ((^), quotRem)

import Data.Ordinal.Positive.Internal
import Data.Ordinal.Finite.Internal
import Data.Ordinal.Minus
import Data.Ordinal.Pow -- maybe use SubHask?
import Data.Ordinal.QuotRem
import Data.Ordinal.Zero
import Data.Ordinal.Lens
import Data.Ordinal.Notation

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

instance forall a. (Typeable a, Show a, HasZero a, Minus a, Num a, Ord a) => 
    Show (Expansion a) where
  showsPrec = \p -> showTerms p . getExpansion where
    showLimit = notation (Infinity @a)

    showTerms _ []  = showString "0"
    showTerms p [q] = showTerm p q
    showTerms p qs  = showParen (p >= 6) $ 
      foldr1 (\x y -> x . showString " + " . y) $ showTerm 6 <$> qs

    showTerm p (Zero, Positive a) = showsPrec p a
    showTerm p (One, Positive 1) = showLimit p
    showTerm p (One, Positive a) = showParen (p >= 7) $
      showLimit 7 . showString " * " . showsPrec 7 a
    showTerm p (Expansion qs, Positive 1) = showParen (p > 8) $ 
      showLimit 8 . showString " ^ " . showTerms 8 qs
    showTerm p (Expansion qs, Positive a) = showParen (p >= 7) $ 
      showLimit 8 . showString " ^ " . showTerms 8 qs . showString " * " . showsPrec 7 a

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
instance (Ord a, Num a, Minus a, LensFinite a, HasZero a) => Num (Expansion a) where
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
    --  = α * (b_0' + n)
    --  = α * b_0' + α * n
    --  = (∞ ^ α_k * a_k + α') * b_0' + (∞ ^ α_k * a_k + α') * n
    --  = ∞ ^ α_k * a_k * b_0' + ∞ ^ α_k * a_k * n + α' * (n > 0)
    --  = ∞ ^ α_k * a_k * (b_0' + n) + α' * (n > 0)
    --  = ∞ ^ α_k * a_k * b_0 + α' * (n > 0)
    go (Zero, b_0) ~[] = (α_k, a_k * b_0) : case viewFinite (getPositive b_0) of 
      (0, _)  -> [] 
      _       -> pt

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
    LessThanBy _ -> error "subtraction is not closed on Expansion numbers"
    EqualTo      -> Zero
    GreaterThanBy γ  -> γ

  negate = error "negation is not defined for Expansion numbers"
  abs = id
  signum Zero = Zero
  signum _ = One
  fromInteger n = fromBase $ fromInteger n  where

-- α `quotRem` β = (γ, δ)
-- α = β * γ + δ
--
instance (Minus a, HasZero a, LensFinite a, Ord a, QuotRem a) => QuotRem (Expansion a) where
  _ `quotRem` Expansion [] = error "division by zero"
  α `quotRem` Expansion (q@(β_j, Positive b_j):qt) = loop $ getExpansion α where

    -- Notation:
    --    α = ∞ ^ α_i * a_i + α' = Expansion ps
    --      where α' < ∞ ^ α_i
    --    β = ∞ ^ β_j * b_j + β' = Expansion qs
    --      where β' < ∞ ^ β_j
    --
    --    (α / β, α % β) = α `quotRem` β
    --      where α = β * (α / β) + (α % β)
    --        and α % β < β
    --
    loop ps@((α_i, Positive a_i):pt) = case α_i `minus` β_j of
      GreaterThanBy γ -> -- => α_i = β_j + γ, γ > 0
        -- => β * ∞ ^ γ
        --    = (∞ ^ β_j * b_j + β') * ∞ ^ γ
        --    = ∞ ^ (β_j + γ)
        --    = ∞ ^ α_i
        -- => α = β * (∞ ^ γ * a_i + α' / β) + α' % β
        case loop pt of ~(Expansion rs, δ) -> (Expansion $ (γ, Positive a_i):rs, δ)
      LessThanBy _ -> -- => β_j = α_i + γ, γ > 0
        -- => α_i < β_j
        -- => α < β
        -- => (α / β, α % β) = (0, α)
        (Zero, Expansion ps)
      EqualTo -> -- => α_i = β_j
        case a_i `quotRem` b_j of
          (Zero, _) -> -- => a_k < b_j
            -- => α < β
            -- => (α / β, α % β) = (0, α)
            (Zero, Expansion ps)
          (c, Zero) -> -- => a_k = b_j * c
            case lensFinite (\case 0 -> Nothing ; n -> Just (n-1)) c of
              Just c' -> -- => c is partially finite (i.e. there exists c' s.t. c' + 1 = c)
                case Expansion pt `minus` Expansion qt of
                  EqualTo -> -- => β' = α'
                    -- => β * c
                    --    = (∞ ^ α_i * b_j + α') * c
                    --    = ∞ ^ α_i * (b_j * c) + α' [c is partially finite]
                    --    = ∞ ^ α_i * a_i + α'
                    --    = α
                    --  => (α / β, α % β) = (c, 0)
                    (Lifted c, Zero)
                  GreaterThanBy δ -> -- => α' = β' + δ, δ > 0
                    -- => β * c + δ
                    --    = (∞ ^ α_i * b_j + β') * c + δ
                    --    = ∞ ^ α_i * (b_j * c) + β' + δ [c is partially finite]
                    --    = ∞ ^ α_i * a_i + α'
                    --    = α
                    --  => (α / β, α % β) = (c, δ)
                    (Lifted c, δ)
                  LessThanBy _ -> -- => β' = α' + δ, δ > 0
                    if c' == Zero
                      then  -- => c = 1
                            -- => a_i = b_j
                            -- => α < β
                            -- => (α / β, α % β) = (0, α)
                            (Zero, Expansion ps)
                      else  -- => c' > 0
                            -- => β * c' + (∞ ^ α_i + α')
                            --    = (∞ ^ α_i * b_j + β') * c' + (∞ ^ α_i * b_j + α')
                            --    = (∞ ^ α_i * (b_j * c') + ...) + (∞ ^ α_i * b_j + α')
                            --    = ∞ ^ α_i * (b_j * c' + b_j) + α'
                            --    = ∞ ^ α_i * (b_j * (c' + 1)) + α'
                            --    = ∞ ^ α_i * (b_j * c) + α'
                            --    = ∞ ^ α_i * a_j + α'
                            --    = α
                            -- => (α / β, α % β) = (c', ∞ ^ α_i * b_j + α')
                            (Lifted c', Expansion $ q:pt)

                  -- lensFinite (\case 0 -> Nothing ; n -> Just (n - 1)) ~ Maybe (c - 1)
              Nothing -> -- => c is purely transfinite (i.e. there is no c' s.t. c' + 1 = c)
                -- => β * c
                --    = (∞ ^ α_i * b_j + β') * c
                --    = ∞ ^ α_i * (b_j * c) [c is purely transfinite]
                --    = ∞ ^ α_i * (b_j * c)
                --    = ∞ ^ α_i * a_k
                -- => (α / β, α % β) = (c, α')
                (Lifted c, Expansion pt)
          (c, d) ->
            -- => α_i = β_j, a_k = b_j * c + d, c > 0, d > 0
            -- => β * c + (∞ ^ α_i * d + α')
            --    = (∞ ^ α_i * b_j + β') * c + (∞ ^ α_i * d + α')
            --    = (∞ ^ α_i * (b_j * c) + β')  + (∞ ^ α_i * d + α')
            --    = ∞ ^ α_i * (b_j * c + d) + α'
            --    = ∞ ^ α_i * a_k + α'
            --    = α
            -- => (α / β, α % β) = (c, ∞ ^ α_i * d + α')
            (Lifted c, Expansion $ (α_i,Positive d):pt)
    loop [] = (Zero,Zero)

instance (Ord a, Minus a) => Minus (Expansion a) where
  Expansion [] `minus` Expansion [] = EqualTo
  Expansion [] `minus` β = LessThanBy β
  α `minus` Expansion [] = GreaterThanBy α
  Expansion ps@((α_i, a_i):pt) `minus` Expansion qs@((β_j, b_j):qt) = case α_i `compare` β_j of
      LT -> LessThanBy $ Expansion qs
      GT -> GreaterThanBy $ Expansion ps 
      EQ -> case a_i `minus` b_j of
        LessThanBy c -> LessThanBy . Expansion $ (β_j, c) : qt
        GreaterThanBy c  -> GreaterThanBy . Expansion $ (α_i, c) : pt
        EqualTo -> Expansion pt `minus` Expansion qt

instance (Ord a, Num a, Minus a, Pow a, HasZero a, LensFinite a) => Pow (Expansion a) where
  _ ^ Expansion [] = One
  Expansion [] ^ _ = Zero

  α ^ One = α
  One ^ _ = One

  -- a ^ (Infinity * β' + b)
  --  = (a ^ Infinity) ^ β' * (a ^ b)
  --  = Infinity ^ β' * (a ^ b)
  Lifted a ^ Expansion qs = Expansion [(Expansion qs', c)] where
    (qs', c) = foldr go ([], 1) qs
    -- β = (Expansion qs)@(∞ ^ β_k * b_k + ... + ∞ ^ β_1 * b_1) + b
    -- β' = ∞ ^ β_k' * b_k + ... + ∞ ^ β_1' * b_1
    --  where 1 + β_i' = β_i
    go q@(β_i, b_i) = case lensFinite (,1) β_i of 
      (0, Zero) -> second . const $ Positive a ^ b_i
      (n, Zero) -> first ((fromFinite (n - 1) , b_i):)
      _         -> first (q:)

  Expansion ((α_k,a_k):pt) ^ β = Expansion $ loop Nothing (Positive 1) γs where
    γs = (α_k *) <$> lensFinite (\(Finite i) -> Finite <$> [i, i-1 .. 0]) β

    loop _ c [γ] = [(γ, c)]
    loop a_k' c (γ:γt) = (γ, fromMaybe a_k a_k') : foldr (go γt a_k' c) [] pt
    loop _ _ [] = error "invalid finite term"

    go γt a_k' _ (Zero, a_0) _ = loop (a_k' <|> Just (a_k * a_0)) a_0 γt
    go (γ':_) _ _ (α_i, a_i) vs = (γ' + α_i, a_i) : vs
    go _ _ _ (_, _) _ = error "invalid finite term"

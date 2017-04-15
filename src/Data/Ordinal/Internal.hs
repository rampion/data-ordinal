{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Internal where
{-------------------------------------------------------------------------------
 Notation
   *  α,β,γ - arbitrary ordinals
   *  a,b,c - arbitrary positive integers
   *  x,y,z - arbitrary non-negative integers
   *  p,q,v - arbitrary (ordinal,positive) pairs
-------------------------------------------------------------------------------}

import Data.Maybe (fromMaybe)
import qualified Data.Ordinal.Positive as P
import qualified Data.Ordinal.NonNegative as N
import Data.Ordinal.LPred

-- | Invariant:
-- > α = Ordinal αs 
-- >   = Ordinal [(α_k,a_k),...,(α_1,a_1),(α_0,a_0)]
-- >   = ω ^ α_k * a_k + ... + ω ^ α_1 * a_1 + ω ^ α_0 * a_0
-- s.t. 
-- >   α_k > ... > α_1 * a_1 > α_0
-- This encoding of α is unique and is referred to as its
-- "Cantor Normal Form" or CNF
newtype Ordinal a = Ordinal { toCNF :: CNF a }
  deriving (Eq, Ord)
type CNF a = [(Ordinal a, P.Positive a)]

fromCNF :: CNF a -> Ordinal a
fromCNF = Ordinal

pattern Zero :: Ordinal a
pattern Zero = Ordinal []

pattern One :: (Eq a, Num a) => Ordinal a
pattern One = Finite 1

pattern Finite :: (Eq a, Num a) => a -> Ordinal a
pattern Finite a = Positive (P.Positive a)

pattern Positive :: P.Positive a -> Ordinal a
pattern Positive a = Ordinal [(Zero, a)]

pattern Omega :: (Eq a, Num a) => Ordinal a
pattern Omega = Ordinal [(One, P.Positive 1)]

fromNonNegative :: (Eq a, Num a) => N.NonNegative a -> Ordinal a
fromNonNegative (N.NonNegative 0) = Zero
fromNonNegative (N.NonNegative a) = Ordinal [(Zero, P.Positive a)]

fromPositive :: (Eq a, Num a) => P.Positive a -> Ordinal a
fromPositive = Positive

toOrdinal :: (Ord a, Num a) => a -> Maybe (Ordinal a)
toOrdinal = fmap fromNonNegative . N.toNonNegative

-- | Incomplete: Ordinal is only a near-semiring
--    * @(-)@ is undefined
--    * @negate@ is undefined
--    * @fromInteger@ is partial
instance (Ord a, Num a) => Num (Ordinal a) where
  (+) = apply mergeAppend
  (*) = apply times
  (-) = error "subtraction is not defined for Ordinal numbers"
  negate = error "negation is not defined for Ordinal numbers"
  abs = id
  signum Zero = Zero
  signum _ = One
  fromInteger n = fromMaybe (error msg) . toOrdinal $ fromInteger n  where
    msg = shows n " can not be converted to a Ordinal number"

instance (Eq a, Num a, LPred a) => LPred (Ordinal a) where
  lpred (P.Positive (Positive a)) = N.NonNegative $ case lpred a of
    N.NonNegative 0 -> Zero
    N.NonNegative a' -> Finite a'
  lpred (P.Positive α) = N.NonNegative α

apply :: (CNF a -> CNF a -> CNF a) -> Ordinal a -> Ordinal a -> Ordinal a
apply f (Ordinal ps) (Ordinal qs) = Ordinal $ f ps qs

-- |
-- Without loss of generality, let
-- >  α' = ω ^ α_j' * x_j' + ... + ω ^ α_j * x_j  
-- >  α = α' + ω ^ α_{j+1} * a_{j+1} + .. + ω ^ α_0 * a_0
-- >  β has CNF ω ^ β_k * b_k + β'
-- such that
-- >  x_i are arbitrary non-negatives satisfying
-- >    x_i' > 0 and i' > i => x_i > 0
-- >  a_i are arbitrary positives
-- >  α_i are arbitrary ordinals satisfying
-- >    α_j' > ... > α_j > α_{j+1} > ... > α_0
-- >  β_k = α_j
-- >  b_k is an arbitrary positive
-- >  β' is an arbitrary ordinal satisfying ω ^ β_k > β'
-- Then
-- >  α + β = α' + β = ω ^ α_j' * x_j' + ... + ω ^ α_j * (x_j + b_k) + β'
--
-- So to put α + β in CNF, 
--  * get the CNF for α' by dropping all terms from the tail of α whose exponent
--    is less than the leading exponent of α
--  * concatenate the CNF of α' and β, merging the last term of α' and the
--    head of β if their exponents are equal
mergeAppend :: (Ord a, Num a) => CNF a -> CNF a -> CNF a
mergeAppend ps [] = ps
mergeAppend ps qs@((β_k,b_k):qt) = fromMaybe qs $ foldr go Nothing ps where
  -- Given β is ω ^ β_k * b_k + β' in CNF
  go p@(α_i,a_i) Nothing = case compare α_i β_k of
    -- α_i < β_k => ω ^ α_i * a_i + β has CNF β
    LT -> Nothing 
    -- α_i = β_k => ω ^ α_i * a_i + β has CNF ω ^ α_i * (a_i + b_k) + β'
    EQ -> Just $ (α_i,a_i + b_k) : qt
    -- α_i > β_k => ω ^ α_i * a_i + β is in CNF
    GT -> Just $ p : qs
  -- a_i >= β_k and i' > i
  --  => a_i' > β_k
  -- ω ^ α_(i'-1) * a_(i'-1)+ ... + ω ^ α_0 * a_0 + β has CNF γ
  --  => ω ^ α_i' * a_i' + ... + ω ^ α_0 * a_0 + β has CNF ω ^ α_i' * a_i' + γ
  go p (Just vs) = Just (p:vs)

times :: (Ord a, Num a) => CNF a -> CNF a -> CNF a
times [] = const []
times ((α_k,a_k):pt) = foldr go [] where
  go (Zero,b_0) _ = (α_k, a_k * b_0) : pt
  go (β_i,b_i) vs = (α_k + β_i, b_i) : vs

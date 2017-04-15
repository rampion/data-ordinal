{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Internal where

import qualified Data.Ordinal.Positive as P
import qualified Data.Ordinal.NonNegative as N

-- | Invariant:
-- > α = Ordinal αs 
-- >   = Ordinal [(α_k,a_k),...,(α_1,a_1),(α_0,a_0)]
-- >   = ω ^ α_k * a_k + ... + ω ^ α_1 * a_1 + ω ^ α_0 * a_0
-- > s.t. 
-- >   α_k > ... > α_1 * a_1 > α_0
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

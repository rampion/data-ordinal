{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal.Internal where

import qualified Data.Ordinal.Positive as P
--import qualified Data.Ordinal.NonNegative as N

type CNF a = [(Ordinal a, P.Positive a)]
newtype Ordinal a = Ordinal { toCNF :: CNF a }

fromCNF :: CNF a -> Ordinal a
fromCNF = Ordinal

pattern Zero :: Ordinal a
pattern Zero = Ordinal []

pattern One :: (Eq a, Num a) => Ordinal a
pattern One = Finite 1

pattern Finite :: a -> Ordinal a
pattern Finite a = Positive (P.Positive a)

pattern Positive :: P.Positive a -> Ordinal a
pattern Positive a = Ordinal [(Zero, a)]

pattern Omega :: (Eq a, Num a) => Ordinal a
pattern Omega = Ordinal [(One, P.Positive 1)]

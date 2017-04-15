{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal
  ( I.Ordinal(), pattern Ordinal, pattern I.Zero, pattern I.One, pattern I.Finite, pattern I.Omega
  , I.CNF, I.toCNF, I.fromCNF
  , P.Positive(), pattern Positive, P.fromPositive, P.toPositive
  , N.NonNegative(), pattern NonNegative, N.fromNonNegative, N.toNonNegative
  ) where

import qualified Data.Ordinal.Internal as I
import qualified Data.Ordinal.Positive as P
import qualified Data.Ordinal.NonNegative as N
-- import Data.Ordinal.Show

pattern Ordinal :: I.CNF a -> I.Ordinal a
pattern Ordinal cnf <- I.Ordinal cnf

pattern Positive :: a -> P.Positive a
pattern Positive a <- P.Positive a

pattern NonNegative :: a -> N.NonNegative a
pattern NonNegative a <- N.NonNegative a

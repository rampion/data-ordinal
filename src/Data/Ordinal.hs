{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal
  ( I.Ordinal(), pattern Ordinal, pattern I.Zero, pattern I.One, pattern I.Finite, pattern I.Omega
  , I.CNF, I.toCNF, I.fromCNF
  , D.Positive(), pattern Positive, D.fromPositive, D.toPositive
  , D.NonNegative(), pattern NonNegative, D.fromNonNegative, D.toNonNegative
  ) where

import qualified Data.Ordinal.Internal as I
import qualified Data.Ordinal.Positive as D
import qualified Data.Ordinal.NonNegative as D
-- import Data.Ordinal.Show

pattern Ordinal :: I.CNF a -> I.Ordinal a
pattern Ordinal cnf <- I.Ordinal cnf

pattern Positive :: a -> D.Positive a
pattern Positive a <- D.Positive a

pattern NonNegative :: a -> D.NonNegative a
pattern NonNegative a <- D.NonNegative a

{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal
  ( I.Expansion(), I.getExpansion, I.toExpansion, pattern Expansion
  , pattern I.Zero, pattern I.One, pattern I.Infinity, pattern I.Omega, pattern I.EpsilonNaught
  , I.lensNonNegative , I.fromNonNegative, I.viewNonNegative , I.degree
  , P.Positive(), pattern Positive, P.getPositive, P.toPositive
  , N.NonNegative(), pattern NonNegative, N.getNonNegative, N.toNonNegative
  , Pow(..)
  , module Data.Ordinal.Finite
  ) where

import qualified Data.Ordinal.Internal as I
import qualified Data.Ordinal.Positive as P
import qualified Data.Ordinal.NonNegative as N
import Data.Ordinal.Pow
import Data.Ordinal.Finite

pattern Expansion :: [(I.Expansion a, P.Positive a)] -> I.Expansion a
pattern Expansion ps <- I.Expansion ps

pattern Positive :: a -> P.Positive a
pattern Positive a <- P.Positive a

pattern NonNegative :: a -> N.NonNegative a
pattern NonNegative a <- N.NonNegative a

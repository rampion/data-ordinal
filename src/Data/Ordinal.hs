{-# LANGUAGE PatternSynonyms #-}
module Data.Ordinal
  ( Finite(getFinite), pattern Finite, toFinite
  , Positive(getPositive), pattern Positive, toPositive
  , Expansion(getExpansion), pattern Expansion, pattern Lifted, pattern Infinity
  , Kleene(Point), pattern Lower, lower
  , FromKleene(FromKleene), fromKleene
  , IsKleene(context), toKleene
  , HasZero(isZero,zero), pattern Zero
  , Pow((^))
  , Minus(minus), Diff(LessThanBy, EqualTo, GreaterThanBy)
  , QuotRem(quotRem)
  , pattern Omega, isOmega, ω
  , pattern OmegaOmega, isOmegaOmega, ω_ω
  , pattern Epsilon, matchEpsilon, epsilon, ε
  , pattern Eps, matchEps, toEps
  , type (<=)(isLTE)
  , Nat(NZero, NSucc)
  , SNat(SZero, SSucc)
  , LTE(LTrian, LEqual, LInfin)
  , MSucc
  , HasEpsilon(MaxEpsilon)
  ) where

import Data.Ordinal.Finite
import Data.Ordinal.Positive
import Data.Ordinal.Expansion 
import Data.Ordinal.Kleene
import Data.Ordinal.Zero
import Data.Ordinal.Pow
import Data.Ordinal.Minus
import Data.Ordinal.QuotRem
import Data.Ordinal.Omega
import Data.Ordinal.OmegaOmega
import Data.Ordinal.Epsilon

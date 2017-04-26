{-# LANGUAGE DeriveFunctor #-}
module Data.Ordinal.Minus where

data Diff a = RightDiff a | NoDiff | LeftDiff a
  deriving Functor

class Minus a where
  -- |
  -- α `minus` β = RightDiff β' <=> α < β, α + β' = β
  -- α `minus` β = NoDiff       <=> α = β
  -- α `minus` β = LeftDiff α'  <=> α > β, α = β + β'
  minus :: a -> a -> Diff a

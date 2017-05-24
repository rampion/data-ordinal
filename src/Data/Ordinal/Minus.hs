{-# LANGUAGE DeriveFunctor #-}
module Data.Ordinal.Minus where

data Diff a = LessThanBy a | EqualTo | GreaterThanBy a
  deriving Functor

class Minus a where
  -- |
  -- α `minus` β = LessThanBy γ     <=> α < β, α + γ = β
  -- α `minus` β = EqualTo          <=> α = β
  -- α `minus` β = GreaterThanBy γ  <=> α > β, α = β + γ
  minus :: a -> a -> Diff a

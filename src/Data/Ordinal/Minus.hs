module Data.Ordinal.Minus where

{- positive a ? -}
data Diff a = RightDiff a | NoDiff | LeftDiff a

class Minus a where
  -- |
  -- α `minus` β = RightDiff β' <=> α < β, α + β' = β
  -- α `minus` β = NoDiff       <=> α = β
  -- α `minus` β = LeftDiff α'  <=> α > β, α = β + β'
  minus :: a -> a -> Diff a

instance Minus Integer where
  a `minus` b = case a `compare` b of
      LT -> RightDiff $ b - a
      EQ -> NoDiff
      GT -> LeftDiff $ a - b

module Data.Ordinal.Positive.Internal where

import Prelude hiding (map, (^))
import Data.Maybe (fromMaybe)

import Data.Ordinal.Minus
import Data.Ordinal.Pow

-- | Invariant: Positive a => a > 0
newtype Positive a = Positive { getPositive :: a }
  deriving (Eq, Ord, Show)

toPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
toPositive a | a <= 0    = Nothing
             | otherwise = Just $ Positive a

-- | Incomplete: Positive is almost a near-semiring
--    * @(-)@ is partial
--    * @negate@ is undefined
--    * @fromInteger@ is partial
instance (Num a, Ord a, Minus a) => Num (Positive a) where
  (+) = apply (+)
  (*) = apply (*)
  a - b = case a `minus` b of 
    LeftDiff c -> c
    _ -> error "subtraction is not closed on Positive numbers"
  negate = error "negation is not defined for Positive numbers"
  abs = id
  signum = map signum
  fromInteger n = fromMaybe (error msg) . toPositive $ fromInteger n  where
    msg = shows n " can not be converted to a Positive number"

instance Pow a => Pow (Positive a) where
  (^) = apply (^)

instance Minus a => Minus (Positive a) where
  Positive a `minus` Positive b = case a `minus` b of
    RightDiff c -> RightDiff $ Positive c
    NoDiff      -> NoDiff
    LeftDiff c  -> LeftDiff $ Positive c

apply :: (a -> a -> a) -> Positive a -> Positive a -> Positive a
apply f (Positive a) (Positive b) = Positive $ f a b

map :: (a -> a) -> Positive a -> Positive a
map f (Positive a) = Positive $ f a

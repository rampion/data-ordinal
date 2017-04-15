module Data.Ordinal.Positive where

import Prelude hiding (map)
import Data.Maybe (fromMaybe)

-- | Invariant: Positive a => a > 0
newtype Positive a = Positive { getPositive :: a }
  deriving (Eq, Ord)

toPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
toPositive a | a <= 0    = Nothing
             | otherwise = Just $ Positive a

-- | Incomplete: Positive is almost a near-semiring
--    * @(-)@ is undefined
--    * @negate@ is undefined
--    * @fromInteger@ is partial
instance (Num a, Ord a) => Num (Positive a) where
  (+) = apply (+)
  (*) = apply (*)
  (-) = error "subtraction is not defined for Positive numbers"
  negate = error "negation is not defined for Positive numbers"
  abs = id
  signum = map signum
  fromInteger n = fromMaybe (error msg) . toPositive $ fromInteger n  where
    msg = shows n " can not be converted to a Positive number"

apply :: (a -> a -> a) -> Positive a -> Positive a -> Positive a
apply f (Positive a) (Positive b) = Positive $ f a b

map :: (a -> a) -> Positive a -> Positive a
map f (Positive a) = Positive $ f a

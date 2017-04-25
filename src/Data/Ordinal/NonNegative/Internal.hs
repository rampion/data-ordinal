module Data.Ordinal.NonNegative.Internal where

import Prelude hiding (map, (^))
import Data.Maybe (fromMaybe)
import Data.Ordinal.Pow

-- | Invariant: NonNegative x => x >= 0
newtype NonNegative a = NonNegative { getNonNegative :: a }
  deriving (Eq, Ord)

toNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
toNonNegative a | a < 0     = Nothing
                | otherwise = Just $ NonNegative a

-- | Incomplete: NonNegative is only a near-semiring
--    * @(-)@ is undefined
--    * @negate@ is undefined
--    * @fromInteger@ is partial
instance (Num a, Ord a) => Num (NonNegative a) where
  (+) = apply (+)
  (*) = apply (*)
  (-) = error "subtraction is not defined for NonNegative numbers"
  negate = error "negation is not defined for NonNegative numbers"
  abs = id
  signum = map signum
  fromInteger n = fromMaybe (error msg) . toNonNegative $ fromInteger n  where
    msg = shows n " can not be converted to a NonNegative number"

instance Pow a => Pow (NonNegative a) where
  (^) = apply (^)

apply :: (a -> a -> a) -> NonNegative a -> NonNegative a -> NonNegative a
apply f (NonNegative a) (NonNegative b) = NonNegative $ f a b

map :: (a -> a) -> NonNegative a -> NonNegative a
map f (NonNegative a) = NonNegative $ f a

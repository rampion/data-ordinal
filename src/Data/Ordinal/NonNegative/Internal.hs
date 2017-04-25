module Data.Ordinal.NonNegative.Internal where

import Prelude hiding (map, (^))
import Data.Maybe (fromMaybe)

import Data.Ordinal.Minus
import Data.Ordinal.Pow

-- | Invariant: NonNegative x => x >= 0
newtype NonNegative a = NonNegative { getNonNegative :: a }
  deriving (Eq, Ord)

toNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
toNonNegative a | a < 0     = Nothing
                | otherwise = Just $ NonNegative a

-- | Incomplete: NonNegative is only a near-semiring
--    * @(-)@ is partial
--    * @negate@ is undefined
--    * @fromInteger@ is partial
instance (Minus a, Num a, Ord a) => Num (NonNegative a) where
  (+) = apply (+)
  (*) = apply (*)
  a - b = case a `minus` b of
    RightDiff _ -> error "subtraction is not closed on NonNegative numbers"
    NoDiff      -> NonNegative 0
    LeftDiff c  -> c
  negate = error "negation is not defined for NonNegative numbers"
  abs = id
  signum = map signum
  fromInteger n = fromMaybe (error msg) . toNonNegative $ fromInteger n  where
    msg = shows n " can not be converted to a NonNegative number"

instance Pow a => Pow (NonNegative a) where
  (^) = apply (^)

instance Minus a => Minus (NonNegative a) where
  NonNegative a `minus` NonNegative b = case a `minus` b of
    RightDiff c -> RightDiff $ NonNegative c
    NoDiff      -> NoDiff
    LeftDiff c  -> LeftDiff $ NonNegative c

apply :: (a -> a -> a) -> NonNegative a -> NonNegative a -> NonNegative a
apply f (NonNegative a) (NonNegative b) = NonNegative $ f a b

map :: (a -> a) -> NonNegative a -> NonNegative a
map f (NonNegative a) = NonNegative $ f a

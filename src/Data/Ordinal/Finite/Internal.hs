{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Ordinal.Finite.Internal where

import Prelude hiding (map, (^))
import qualified Prelude

import Data.Maybe (fromMaybe)

import Data.Ordinal.Minus
import Data.Ordinal.Pow
import Data.Ordinal.Zero
import Data.Ordinal.LPred
import Data.Ordinal.Positive

-- | Invariant: Finite x => x >= 0
newtype Finite = Finite { getFinite :: Integer }
  deriving (Eq, Ord, Enum)

toFinite :: Integer -> Maybe Finite
toFinite a | a < 0     = Nothing
           | otherwise = Just $ Finite a

instance Show Finite where
  showsPrec p = showsPrec p . getFinite

-- | Incomplete: Finite is only a near-semiring
--    * @(-)@ is partial
--    * @negate@ is undefined
--    * @fromInteger@ is partial
instance Num Finite where
  (+) = apply (+)
  (*) = apply (*)
  a - b = case a `minus` b of
    RightDiff _ -> error "subtraction is not closed on Finite numbers"
    NoDiff      -> Finite 0
    LeftDiff c  -> c
  negate = error "negation is not defined for Finite numbers"
  abs = id
  signum = map signum
  fromInteger n = fromMaybe (error msg) . toFinite $ fromInteger n  where
    msg = shows n " can not be converted to a Finite number"

instance Minus Finite where
  Finite a `minus` Finite b = Finite <$> case a `compare` b of
      LT -> RightDiff $ b - a
      EQ -> NoDiff
      GT -> LeftDiff $ a - b

instance HasZero Finite where
  isZero (Finite 0) = True
  isZero _ = False
  zero = Finite 0

instance LPred Finite where
  lpred = map pred . getPositive

instance Pow Finite where
  (^) = apply (Prelude.^)

apply :: (Integer -> Integer -> Integer) -> Finite -> Finite -> Finite
apply f (Finite a) (Finite b) = Finite $ f a b

map :: (Integer -> Integer) -> Finite -> Finite
map f (Finite a) = Finite $ f a

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Ordinal.Zero where

class HasZero a where
  isZero :: a -> Bool
  zero :: a

pattern Zero :: HasZero a => a
pattern Zero <- (isZero -> True) where Zero = zero

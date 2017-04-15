module Data.Ordinal.NonNegative where

newtype NonNegative a = NonNegative { fromNonNegative :: a }

toNonNegative :: a -> Maybe (NonNegative a)
toNonNegative = undefined

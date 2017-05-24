module Data.Ordinal.QuotRem where

class QuotRem a where
  quotRem :: a -> a -> (a,a)


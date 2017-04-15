module Data.Ordinal.Positive where

newtype Positive a = Positive { fromPositive :: a }

toPositive :: a -> Maybe (Positive a)
toPositive = undefined

module Data.Ordinal.LPred where

import Data.Ordinal.Positive

class LPred a where
  -- | lpred a = a' s.t. 1 + a' = a
  lpred :: Positive a -> a

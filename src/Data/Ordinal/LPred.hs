module Data.Ordinal.LPred where

import Data.Ordinal.Positive
import Data.Ordinal.NonNegative.Internal

class LPred a where
  -- | lpred a = a' s.t. 1 + a' = a
  lpred :: Positive a -> NonNegative a

instance LPred Integer where
  lpred = NonNegative . pred . getPositive

module Data.Ordinal.LPred where

import Prelude hiding (map)
import Data.Ordinal.Positive
import Data.Ordinal.Finite.Internal

class LPred a where
  -- | lpred a = a' s.t. 1 + a' = a
  lpred :: Positive a -> a

instance LPred Finite where
  lpred = map pred . getPositive

module Data.Ordinal.Pow where

import Prelude hiding ((^))
import qualified Prelude

class Pow a where
  (^) :: a -> a -> a

instance Pow Integer where
  (^) = (Prelude.^)


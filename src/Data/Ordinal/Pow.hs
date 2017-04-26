module Data.Ordinal.Pow where

import Prelude hiding ((^))
import qualified Prelude

import Data.Ordinal.Finite.Internal

class Pow a where
  (^) :: a -> a -> a

instance Pow Finite where
  (^) = apply (Prelude.^)


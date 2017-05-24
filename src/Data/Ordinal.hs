module Data.Ordinal
  ( module Ordinal
  , Countable
  ) where

import Prelude hiding ((^),quotRem)
import Data.Ordinal.Finite as Ordinal
import Data.Ordinal.Positive as Ordinal
import Data.Ordinal.Expansion as Ordinal
import Data.Ordinal.Kleene as Ordinal
import Data.Ordinal.Zero as Ordinal
import Data.Ordinal.Pow as Ordinal
import Data.Ordinal.Lens as Ordinal
import Data.Ordinal.Minus as Ordinal
import Data.Ordinal.QuotRem as Ordinal
import Data.Ordinal.Omega as Ordinal
import Data.Ordinal.OmegaOmega as Ordinal
import Data.Ordinal.Epsilon as Ordinal
import Data.Ordinal.Notation as Ordinal

-- | the closure of @Finite@ under @Expansion@
type Countable = Kleene Expansion Finite

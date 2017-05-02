{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Ordinal.Omega where

import Data.Ordinal.Finite
import Data.Ordinal.Expansion.Internal
import Data.Ordinal.Kleene.Internal
import Data.Ordinal.Lens

-- | polymorphic constructor for the smallest transfinite number
pattern Omega :: HasOmega a => a
pattern Omega <- (isOmega -> True)
  where Omega = ω

class HasOmega a where
  isOmega :: a -> Bool
  ω :: a

-- base cases
instance HasOmega (Expansion Finite) where
  isOmega Infinity = True
  isOmega _ = False
  ω = Infinity
instance HasOmega (Kleene Expansion Finite) where
  isOmega (Lower (Point Omega)) = True
  isOmega _ = False
  ω = (Lower (Point Omega))

-- define instances for @HasOmega a => HasOmega (Expansion a)@ while
-- avoiding overlap with the @HasOmega (Expansion Finite)@ instance
instance HasOmega (Expansion a) => HasOmega (Expansion (Expansion a)) where
  isOmega (Lifted Omega) = True
  isOmega _ = False
  ω = Lifted Omega
instance HasOmega (Kleene t b) => HasOmega (Expansion (Kleene t b)) where
  isOmega (Lifted Omega) = True
  isOmega _ = False
  ω = Lifted Omega

-- define instances for @HasOmega (Klenee (Kleene+ Expansion) Finite)@
instance (LensBase t, HasOmega (Kleene t Finite)) => HasOmega (Kleene (Kleene t) Finite) where
  isOmega (Lower (Point Omega)) = True
  isOmega _ = False
  ω = Lower (Point Omega)

-- define instances for @HasOmega a => HasOmega (Kleene t a)@, while avoiding
-- overlap with Kleene (Kleene* Expansion) Finite
instance (LensBase t, LensFinite b, Derived b, HasOmega (Expansion b)) => HasOmega (Kleene t (Expansion b)) where
  isOmega (Point Omega) = True
  isOmega _ = False
  ω = Point Omega
instance (LensBase t, Derived b, HasOmega (Kleene t b)) => HasOmega (Kleene t' (Kleene t b)) where
  isOmega (Point Omega) = True
  isOmega _ = False
  ω = Point Omega

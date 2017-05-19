{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Ordinal.OmegaOmega where
import Data.Typeable

import Data.Ordinal.Finite
import Data.Ordinal.Expansion.Internal
import Data.Ordinal.Kleene.Internal
import Data.Ordinal.Lens

-- | polymorphic constructor for the smallest non-countable transfinite number
pattern OmegaOmega :: HasOmegaOmega a => a
pattern OmegaOmega <- (isOmegaOmega -> True)
  where OmegaOmega = ω_ω

class HasOmegaOmega a where
  isOmegaOmega :: a -> Bool
  ω_ω :: a

-- base cases
instance HasOmegaOmega (Expansion (Kleene Expansion Finite)) where
  isOmegaOmega Infinity = True
  isOmegaOmega _ = False
  ω_ω = Infinity
instance HasOmegaOmega (Kleene Expansion (Kleene Expansion Finite)) where
  isOmegaOmega (Lower (Point OmegaOmega)) = True
  isOmegaOmega _ = False
  ω_ω = Lower (Point OmegaOmega)
instance HasOmegaOmega (Kleene (Kleene Expansion) Finite) where
  isOmegaOmega (Lower (Lower (Point OmegaOmega))) = True
  isOmegaOmega _ = False
  ω_ω = Lower (Lower (Point OmegaOmega))

-- define instances for @HasOmegaOmega a => HasOmegaOmega (Expansion a)@ while
-- avoiding overlap with the @HasOmegaOmega (Expansion (Kleene Expansion Finite))@ instance
instance HasOmegaOmega (Expansion a) => HasOmegaOmega (Expansion (Expansion a)) where
  isOmegaOmega (Lifted OmegaOmega) = True
  isOmegaOmega _ = False
  ω_ω = Lifted OmegaOmega
instance HasOmegaOmega (Kleene t (Expansion a)) => HasOmegaOmega (Expansion (Kleene t (Expansion a))) where
  isOmegaOmega (Lifted OmegaOmega) = True
  isOmegaOmega _ = False
  ω_ω = Lifted OmegaOmega
instance HasOmegaOmega (Kleene t (Kleene t' b)) => HasOmegaOmega (Expansion (Kleene t (Kleene t' b))) where
  isOmegaOmega (Lifted OmegaOmega) = True
  isOmegaOmega _ = False
  ω_ω = Lifted OmegaOmega

-- define instances for @HasOmegaOmega (Klenee (Kleene+ Expansion) (Kleene Expansion Finite))@
instance (Typeable t, LensBase t, HasOmegaOmega (Kleene t (Kleene Expansion Finite))) => 
    HasOmegaOmega (Kleene (Kleene t) (Kleene Expansion Finite)) where
  isOmegaOmega (Lower (Point OmegaOmega)) = True
  isOmegaOmega _ = False
  ω_ω = Lower (Point OmegaOmega)

-- define instances for @HasOmegaOmega (Klenee (Kleene{2,} Expansion) Finite)@
instance (Typeable t, LensBase t, HasOmegaOmega (Kleene (Kleene t) Finite)) => 
    HasOmegaOmega (Kleene (Kleene (Kleene t)) Finite) where
  isOmegaOmega (Lower (Point OmegaOmega)) = True
  isOmegaOmega _ = False
  ω_ω = Lower (Point OmegaOmega)

-- define instances for @HasOmegaOmega a => HasOmegaOmega (Kleene t a)@, while avoiding
-- overlap with Kleene (Kleene* Expansion) (Kleene Expansion Finite)
instance (Typeable t, LensBase t, Derived b, HasOmegaOmega (Kleene (Kleene t) b)) =>
    HasOmegaOmega (Kleene t' (Kleene (Kleene t) b)) where
  isOmegaOmega (Point OmegaOmega) = True
  isOmegaOmega _ = False
  ω_ω = Point OmegaOmega
instance (LensFinite a, Derived a, HasOmegaOmega (Kleene Expansion (Expansion a))) => 
    HasOmegaOmega (Kleene t' (Kleene Expansion (Expansion a))) where
  isOmegaOmega (Point OmegaOmega) = True
  isOmegaOmega _ = False
  ω_ω = Point OmegaOmega
instance (Typeable t, LensBase t, Derived b, HasOmegaOmega (Kleene Expansion (Kleene t b))) => 
    HasOmegaOmega (Kleene t' (Kleene Expansion (Kleene t b))) where
  isOmegaOmega (Point OmegaOmega) = True
  isOmegaOmega _ = False
  ω_ω = Point OmegaOmega
instance (LensFinite a, Derived a, HasOmegaOmega (Expansion a)) =>
    HasOmegaOmega (Kleene t (Expansion a)) where
  isOmegaOmega (Point OmegaOmega) = True
  isOmegaOmega _ = False
  ω_ω = Point OmegaOmega

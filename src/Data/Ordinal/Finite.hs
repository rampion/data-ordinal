{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Ordinal.Finite where
import Data.Ordinal.NonNegative

type Finite = NonNegative Integer

class LensFinite a where
  lensFinite :: Functor f => (Finite -> f Finite) -> a -> f a

instance LensFinite Finite where
  lensFinite = id

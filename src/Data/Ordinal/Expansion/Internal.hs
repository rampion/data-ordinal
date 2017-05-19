-- Purely exists to break the inheritance chain so we can define orphan Show
-- instances for Expansion types.
--
-- These orphans are safe, as the Private module is not exposed, but this
-- module is
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Data.Ordinal.Expansion.Internal
  ( module Private
  ) where

import Data.Ordinal.Expansion.Private as Private
import Data.Ordinal.Kleene
import Data.Ordinal.Kleene.Internal (Context(..))
import Data.Ordinal.Finite

instance Show (Expansion Finite) where
  showsPrec = showExpansion "ω" showsPrec

instance Show (Expansion (Kleene Expansion Finite)) where
  showsPrec = showExpansion "ω_ω" showsPrec

instance forall a. (Show (Expansion a), IsKleene a Expansion Finite) => Show (Expansion (Expansion a)) where
  showsPrec = showExpansion epsilon_n showsPrec where
    epsilon_n :: String
    epsilon_n = "ε_" ++ show n

    n :: Int
    n = loop (context :: Context a Expansion Finite) 0

    loop :: forall x. Context x Expansion Finite -> Int -> Int
    loop (Inductive pf) k = loop pf $! k + 1
    loop (Reflexive) k = k

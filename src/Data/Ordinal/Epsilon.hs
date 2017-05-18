{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Ordinal.Epsilon 
  ( module Data.Ordinal.Epsilon.Internal
  , pattern Epsilon
  , matchEpsilon, epsilon, ε
  , pattern Eps
  , matchEps, toEps
  , Z(..), toZ
  , type (<=)(..)
  ) where

import Control.Monad (join)

import qualified Data.Ordinal.Epsilon.Internal as I
import Data.Ordinal.Epsilon.Internal 
  ( Nat(..)
  , SNat(..)
  , LTE(..)
  , MSucc
  , HasEpsilon(MaxEpsilon)
  )

-- This module defines both Epsilon and Eps because
-- Epsilon is better if you want to make sure the pattern
-- is reachable, but Eps is conveniently succinct.
--
-- This is a compile error:
-- > isEpsilonOne :: Expansion (Expansion Finite) -> Bool
-- > isEpsilonOne (Epsilon LEqual (SSucc SZero)) = True
-- > isEpsilonOne _ = False
--
-- This always returns False:
-- > isEpsOne :: Expansion (Expansion Finite) -> Bool
-- > isEpsOne (Eps 1) = True
-- > isEpsOne _ = False

pattern Epsilon :: HasEpsilon a => LTE m (MaxEpsilon a) -> SNat m -> a
pattern Epsilon pf sm <- (matchEpsilon -> Just (pf :=> sm))
  where Epsilon = epsilon

matchEpsilon :: HasEpsilon a => a -> Maybe (Z (MaxEpsilon a))
matchEpsilon a = I.matchEpsilon I.hasEpsilonD a (:=>)

epsilon :: HasEpsilon a => LTE m (MaxEpsilon a) -> SNat m -> a
epsilon = I.epsilon I.hasEpsilonD

ε :: (HasEpsilon a, m <= MaxEpsilon a) => SNat m -> a
ε = epsilon isLTE
  
pattern Eps :: HasEpsilon a => Int -> a
pattern Eps i <- (matchEps -> Just i)

matchEps :: HasEpsilon a => a -> Maybe Int
matchEps a = (\(_ :=> (I.fromSNat -> i)) -> i) <$> matchEpsilon a where

toEps :: forall a. HasEpsilon a => Int -> Maybe a
toEps = case (I.hasEpsilonD :: I.HasEpsilonD a (MaxEpsilon a)) of
  I.HasEpsilonD { I.maxEpsilon = I.SNothing, I.epsilon = eps } ->
    I.asSNat $ eps LInfin
  I.HasEpsilonD { I.maxEpsilon = I.SJust sn, I.epsilon = eps } ->
    let test :: forall m. SNat m -> Maybe a
        test sm = case I.hasTotalOrder sm sn of
          Left pf -> Just (eps pf sm)
          Right _ -> Nothing
    in join . I.asSNat test
  
toZ :: (m <= n) => SNat m -> Z n
toZ = (isLTE :=>)

-- | Z n ~ { SNat m | m <= n }
data Z (n :: Maybe Nat) where
  (:=>) :: LTE m n -> SNat m -> Z n

instance Eq (Z n) where
  _ :=> sx == _ :=> sy = I.fromSNat sx == I.fromSNat sy

instance Show (Z n) where
  show (_ :=> sm) = show (I.fromSNat sm)

class (m <= n) where
  isLTE :: LTE m n

instance (m <= 'Nothing) where
  isLTE = LInfin

instance ('NZero <= 'Just 'NZero) where
  isLTE = LEqual

instance ('NZero <= 'Just n) => ('NZero <= 'Just ('NSucc n)) where
  isLTE = LTrian isLTE

instance (m <= 'Just n) => ('NSucc m <= 'Just ('NSucc n)) where
  isLTE = I.bisucc isLTE

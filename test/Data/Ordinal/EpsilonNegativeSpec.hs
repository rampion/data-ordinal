{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-} -- needed for shouldNotTypecheck
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.Ordinal.EpsilonNegativeSpec where

import Control.Arrow ((***))

import Test.Hspec
import Test.HUnit.Lang (Assertion)
import Control.DeepSeq (NFData(..))
import Test.ShouldNotTypecheck (shouldNotTypecheck) 

import Data.Ordinal.Expansion
import Data.Ordinal.Finite
import Data.Ordinal.Positive
import Data.Ordinal.Epsilon

import Data.Ordinal.Epsilon.SpecHelper

spec :: Spec
spec = do

  describe "toZ" $ do
    it "can't produce a proof that 1 < 0" $ do
      shouldNotTypecheckWrapped (toZ sOne :: Z ('Just 'NZero))
      -- shouldNotTypecheckWrapped (toZ sOne :: Z ('Just NOne)) -- useful when typechecking rest of file

  {-
  describe "epsilon" $ do
    it "can't produce ε_1 :: Expansion (Expansion Finite)" $ do
      -- XXX: doesn't typecheck, but shouldNotTypecheck can't currently capture it
      --      see https://github.com/CRogers/should-not-typecheck/issues/12
      shouldNotTypecheckWrapped (ε sOne :: EEF)
      -- shouldNotTypecheckWrapped (ε SZero :: EEF) -- useful when typechecking rest of file
  -}

newtype Wrapped a = Wrapped { getWrapped :: a }

-- use a wrapper so we can avoid an extra dependency on deepseq in the library
shouldNotTypecheckWrapped :: NFData (Wrapped a) => (() ~ () => a) -> Assertion
shouldNotTypecheckWrapped a = shouldNotTypecheck (Wrapped a)

instance NFData (Wrapped (Z n)) where
  rnf (Wrapped (pf :=> sm)) = rnf (Wrapped pf) `seq` rnf (Wrapped sm)

instance NFData (Wrapped (LTE m n)) where
  rnf (Wrapped (LTrian pf)) = rnf (Wrapped pf)
  rnf (Wrapped LEqual) = ()
  rnf (Wrapped LInfin) = ()

instance NFData (Wrapped (SNat n)) where
  rnf (Wrapped (SSucc sn)) = rnf (Wrapped sn)
  rnf (Wrapped SZero) = ()

instance NFData (Wrapped a) => NFData (Wrapped (Expansion a)) where
  rnf = rnf . map (Wrapped *** Wrapped) . getExpansion . getWrapped

instance NFData (Wrapped a) => NFData (Wrapped (Positive a)) where
  rnf = rnf . Wrapped . getPositive . getWrapped

instance NFData (Wrapped Finite) where
  rnf = rnf . getFinite . getWrapped

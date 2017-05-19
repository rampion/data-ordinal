{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE GADTs  #-}
module Data.Ordinal.NotationSpec where

import Control.Monad (forM_)
import Data.Typeable

import Test.Hspec
import Data.Ordinal.Finite
import Data.Ordinal.Expansion
import Data.Ordinal.Kleene
import Data.Ordinal.Notation

data Example = forall proxy a. Typeable a => proxy a :=~ String

spec :: Spec
spec = do
  describe "notation" $ do
    let examples = 
          [ Proxy @Finite :=~ "ω"
          , Proxy @(Expansion Finite) :=~ "ε_0"
          , Proxy @(Expansion (Expansion Finite)) :=~ "ε_1"
          , Proxy @(Expansion (Expansion (Expansion Finite))) :=~ "ε_2"
          , Proxy @(Kleene Expansion Finite) :=~ "ω_ω"
          , Proxy @(Kleene Expansion (Expansion (Expansion Finite))) :=~ "ω_ω"
          , Proxy @(Expansion (Kleene Expansion Finite)) :=~ "Infinity @(Expansion (Kleene Expansion Finite))"
          , Proxy @(Expansion (Kleene Expansion (Expansion (Expansion Finite)))) :=~ 
              "Infinity @(Expansion (Kleene Expansion Finite))"
          , Proxy @(Kleene Maybe Integer) :=~ "Infinity @(Kleene Maybe Integer)"
          , Proxy @(Kleene Maybe (Expansion (Kleene Expansion (Expansion (Expansion Finite))))) :=~ 
              "Infinity @(Kleene Maybe (Expansion (Kleene Expansion Finite)))"

          , Proxy @(Kleene (Kleene (Kleene Expansion)) (Kleene Expansion (Kleene (Kleene Expansion) Finite))) :=~
              "Infinity @(Kleene (Kleene (Kleene Expansion)) Finite)"
          ] 
      
    forM_ examples $ \(proxy :=~ expected) ->
      it ("generates " ++ expected ++ " for " ++ show (typeRep proxy)) $
        notation proxy 0 "" `shouldBe` expected

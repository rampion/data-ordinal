module Data.Ordinal.ExpansionSpec where

import Prelude hiding ((^))
import Control.Monad (mapM_)

import Test.Hspec

import Data.Ordinal.Expansion
import Data.Ordinal.Finite
import Data.Ordinal.Pow
import Data.Ordinal.Omega
import Data.Ordinal.Epsilon

spec :: Spec
spec = do
  let opTest :: (Eq a, Show a) => (a -> a -> a) -> String -> Int -> (a,a,a) -> SpecWith ()
      opTest op name prec (lhs, rhs, expected) = 
        foldr ($) ""
        [ showString "computes "
        , showsPrec prec lhs
        , showString " "
        , showString name
        , showString " "
        , showsPrec prec rhs
        , showString " = "
        , showsPrec 0 expected
        ] `it` (lhs `op` rhs `shouldBe` expected)

      opDescribe :: (Eq a, Show a) => (a -> a -> a) -> String -> Int -> [(a,a,a)] -> SpecWith ()
      opDescribe op name prec examples =
        describe name $ mapM_ (opTest op name prec) examples
        
  describe "Num (Expansion Finite)" $ do
    opDescribe (+) "+" 6
      [ (0, 0, 0 :: Expansion Finite)
      , (1, 0, 1)
      , (0, 1, 1)
      , (1, 1, 2) , (ω, 1, Expansion [(1,1), (0,1)]) , (1, ω, ω)
      , (ω, ω ^ 2, ω ^ 2)
      ] 

    opDescribe (*) "*" 7
      [ (0, 0, 0 :: Expansion Finite)
      , (1, 0, 0)
      , (0, 1, 0)
      , (1, 1, 1)
      , (ω, 1, ω)
      , (1, ω, ω)
      , (ω, 2, Expansion [(1,2)])
      , (2, ω, ω)
      , (ω ^ 2, ω, ω ^ 3)
      , (ω, ω ^ 2, ω ^ 3)
      , (ω ^ ω, ω, ω ^ (ω + 1))
      , (ω, ω ^ ω, ω ^ ω)
      ] 

  describe "Pow (Expansion Finite)" $ do
    opDescribe (^) "^" 8
      [ (ω, ω, Expansion [(ω,1)] :: Expansion Finite)
      ]

  describe "Num (Expansion (Expansion Finite))" $ do
    opDescribe (*) "*" 7
      [ (ε SZero + 1, ω, ε SZero * ω :: Expansion (Expansion Finite))
      ]

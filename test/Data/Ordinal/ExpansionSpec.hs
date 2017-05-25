{-# LANGUAGE TypeApplications #-}
module Data.Ordinal.ExpansionSpec where

import Prelude hiding ((^), quotRem)
import Control.Monad (forM_)

import Test.Hspec

import Data.Ordinal.Expansion
import Data.Ordinal.Finite
import Data.Ordinal.Pow
import Data.Ordinal.QuotRem
import Data.Ordinal.Omega
import Data.Ordinal.Epsilon

spec :: Spec
spec = do
  describe "Num (Expansion Finite)" $ do
    opDescribe @(Expansion Finite) (+) "+" 6
      [ (0, 0, 0)
      , (1, 0, 1)
      , (0, 1, 1)
      , (1, 1, 2)
      , (ω, 1, Expansion [(1,1), (0,1)])
      , (1, ω, ω)
      , (ω, ω ^ 2, ω ^ 2)
      ]

    opDescribe @(Expansion Finite) (*) "*" 7
      [ (0, 0, 0)
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

  describe "QuotRem (Expansion Finite)" $ do
    runExamples "quotRem" (opName "`quotRem`" 9)
      [ (0 :: Expansion Finite, 1, (0, 0))
      , (7, 2, (3, 1))
      , (ω, 3, (ω, 0))
      , (ω, ω, (1, 0))
      , (ω + 2, ω, (1, 2))
      , (ω * 2, ω, (2, 0))
      , (ω ^ 2, ω, (ω, 0))
      , (ω ^ 2 + ω * 3 + 4, ω, (ω + 3, 4))
      , (ω ^ 2 * 3, ω * 3, (ω * 3, 0))
      , (1, ω, (0, 1))
      -- α_2 < β_0
      , (ω ^ 2 + ω * 3 + 4, ω ^ ω, (0, ω ^ 2 + ω * 3 + 4)) 
      -- α_2 > β_0, α_1 > β_0, α_0 < β_0
      , (ω ^ ω + ω ^ 2 + 1, ω, (ω ^ ω + ω, 1))
      -- α_0 = β_0, a_0 / b_0 = 0
      , (ω, ω * 2, (0, ω))
      -- α_2 = β_0, a_2 / b_0 = 0,
      , (ω ^ 2 + ω * 3 + 4, ω ^ 2 * 2, (0, ω ^ 2 + ω * 3 + 4))
      -- α_2 = β_0, a_2 `quotRem` b_0 = (2,1)
      , (ω ^ 2 * 5 + ω * 3 + 4, ω ^ 2 * 2 + ω + 2, (2, ω ^ 2 + ω * 3 + 4))
      -- α_2 = β_1, a_2 `quotRem` b_1 = (2,0), α' = β'
      , (ω ^ 2 * 4 + ω  + 2, ω ^ 2 * 2 + ω + 2, (2, 0))
      -- α_2 = β_1, a_2 `quotRem` b_1 = (2,0), α' > β'
      , (ω ^ 2 * 4 + ω * 3 + 4, ω ^ 2 * 2 + ω + 2, (2, ω * 2 + 4))
      -- α_2 = β_1, a_2 `quotRem` b_1 = (2,0), α' < β'
      , (ω ^ 2 * 4 + ω  + 2, ω ^ 2 * 2 + ω * 3 + 4, (1, ω ^ 2 * 2 + ω + 2))
      -- α_2 = β_1, a_2 `quotRem` b_1 = (1,0), α' < β'
      , (ω ^ 2 * 4 + ω  + 2, ω ^ 2 * 4 + ω * 3 + 4, (0, ω ^ 2 * 4 + ω + 2))
      ] $ \(lhs,rhs,expected@(q,r)) -> do
            opProperty quotRem (lhs, rhs, expected)
            rhs * q + r `shouldBe` lhs
            (r < rhs) `shouldBe` True

  describe "Pow (Expansion Finite)" $ do
    opDescribe @(Expansion Finite) (^) "^" 8
      [ (ω, ω, Expansion [(ω,1)])
      ]

  describe "Num (Expansion (Expansion Finite))" $ do
    opDescribe @(Expansion (Expansion Finite)) (*) "*" 7
      [ (ε SZero + 1, ω, ε SZero * ω)
      , (ω, ε SZero + 1, ε SZero + ω)
      ]

  describe "QuotRem (Expansion (Expansion Finite))" $ do
    let ε0 = Infinity :: Expansion (Expansion Finite)
    runExamples "quotRem" (opName "`quotRem`" 9)
      -- α_2 = β_0, a_2 `quotRem` b_0 = (ω,0)
      [  (ε0 ^ 2 * ω + ε0 + 2, ε0 ^ 2 * 3 + ε0 * 3 + 4, (ω, ε0 + 2))
      ] $ \(lhs,rhs,expected@(q,r)) -> do
            opProperty quotRem (lhs, rhs, expected)
            rhs * q + r `shouldBe` lhs
            (r < rhs) `shouldBe` True

type Test = IO ()

runExamples :: String -> (a -> String) -> [a] -> (a -> Test) -> SpecWith ()
runExamples title nameFor examples propertyOf =
  describe title $
    forM_ examples $ \a ->
      it (nameFor a) (propertyOf a)

opDescribe :: (Show a, Show b, Show c, Eq c) => (a -> b -> c) -> String -> Int -> [(a,b,c)] -> SpecWith ()
opDescribe op name prec examples =
  runExamples name (opName name prec) examples (opProperty op)

opName :: (Show a, Show b, Show c) => String -> Int -> (a,b,c) -> String
opName name prec (lhs, rhs, expected) = foldr ($) ""
  [ showString "computes "
  , showsPrec prec lhs
  , showString " "
  , showString name
  , showString " "
  , showsPrec prec rhs
  , showString " = "
  , showsPrec 0 expected
  ] 

opProperty :: (Show c, Eq c) => (a -> b -> c) -> (a, b, c) -> Test
opProperty op (lhs, rhs, expected) = lhs `op` rhs `shouldBe` expected

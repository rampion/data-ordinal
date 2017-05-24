{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Data.Ordinal.EpsilonSpec where
#if 1
spec :: Monad m => m ()
spec = return ()
#else
import Data.Maybe

import Test.Hspec

import Data.Ordinal.Expansion.Internal
import Data.Ordinal.Kleene
import Data.Ordinal.Epsilon
import Data.Ordinal.Epsilon.Internal (fromSNat)

import Data.Ordinal.Epsilon.SpecHelper

spec :: Spec
spec = do
  describe "Epsilon" $ do
    it "can be used as pattern for Expansion (Expansion Finite)" $ do
      let f :: EEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f Infinity `shouldBe` Just 0

    it "can be used as pattern for Expansion (Expansion (Expansion Finite))" $ do
      let f :: EEEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      catMaybes [ f Infinity, f (Lifted Infinity) ] `shouldBe` [1, 0]

    it "can be used as pattern for Expansion (Kleene Expansion Finite)" $ do
      let f :: EKEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f Infinity `shouldBe` Nothing
      f (Lifted .
         Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Expansion (Expansion (Kleene Expansion Finite))" $ do
      let f :: EEKEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f Infinity `shouldBe` Nothing
      f (Lifted Infinity) `shouldBe` Nothing
      f (Lifted . Lifted .
         Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene Expansion Finite" $ do
      let f :: KEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f (Point 0) `shouldBe` Nothing
      f (Lower $ Point Infinity) `shouldBe` Nothing
      f (Lower . Lower $ Point Infinity) `shouldBe` Just 0
      f (Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene Expansion (Expansion Finite)" $ do
      let f :: KEEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f (Point Infinity) `shouldBe` Nothing
      f (Lower $ Point Infinity) `shouldBe` Just 0
      f (Lower . Lower $ Point Infinity) `shouldBe` Just 1
      f (Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 5

    it "can be used as pattern for Kleene Expansion (Expansion (Expansion Finite))" $ do
      let f :: KEEEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f (Point Infinity) `shouldBe` Just 0
      f (Lower $ Point Infinity) `shouldBe` Just 1
      f (Lower . Lower $ Point Infinity) `shouldBe` Just 2
      f (Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 6

    it "can be used as pattern for Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      let f :: KEEKEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f (Point Infinity) `shouldBe` Nothing
      f (Point . Lifted .  Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene Expansion (Kleene Expansion Finite)" $ do
      let f :: KEKEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f (Point . Lower . Lower $ Point Infinity) `shouldBe` Just 0
      f (Point . Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene (Kleene Expansion) Finite" $ do
      let f :: KKEF -> Maybe Int
          f (Epsilon _ sm) = Just (fromSNat sm)
          f _ = Nothing
      f (Lower . Point .  Lower . Lower $ Point Infinity) `shouldBe` Just 0
      f (Lower . Point . Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

  describe "matchEpsilon" $ do

    it "matches ε_0 :: Expansion (Expansion Finite)" $ do
      matchEpsilon (Infinity :: EEF) `shouldBe` Just (toZ SZero)

    it "matches ε_0 :: Expansion (Expansion (Expansion Finite))" $ do
      matchEpsilon (Lifted Infinity :: EEEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Expansion (Expansion (Expansion Finite))" $ do
      matchEpsilon (Infinity :: EEEF) `shouldBe` Just (toZ sOne)

    it "matches ε_0 :: Expansion (Kleene Expansion Finite)" $ do
      matchEpsilon (Lifted . Lower . Lower $ Point Infinity :: EKEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Expansion (Kleene Expansion Finite)" $ do
      matchEpsilon (Lifted . Lower .
                    Lower . Lower $ Point Infinity :: EKEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Expansion (Kleene Expansion Finite)" $ do
      matchEpsilon (Lifted . Lower . Lower .
                    Lower . Lower $ Point Infinity :: EKEF) `shouldBe` Just (toZ sTwo)

    it "matches ε_0 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEpsilon (Lifted . Lifted .
                    Lower . Lower $ Point Infinity :: EEKEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEpsilon (Lifted . Lifted . Lower .
                    Lower . Lower $ Point Infinity :: EEKEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEpsilon (Lifted . Lifted . Lower . Lower .
                    Lower . Lower $ Point Infinity :: EEKEF) `shouldBe` Just (toZ sTwo)

    it "matches ε_0 :: Kleene Expansion Finite" $ do
      matchEpsilon (Lower . Lower $ Point Infinity :: KEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Kleene Expansion Finite" $ do
      matchEpsilon (Lower .
                    Lower . Lower $ Point Infinity :: KEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Kleene Expansion Finite" $ do
      matchEpsilon (Lower . Lower .
                    Lower . Lower $ Point Infinity :: KEF) `shouldBe` Just (toZ sTwo)

    it "matches ε_0 :: Kleene Expansion (Expansion Finite)" $ do
      matchEpsilon (Lower $ Point Infinity :: KEEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Kleene Expansion (Expansion Finite)" $ do
      matchEpsilon (Lower . Lower $ Point Infinity :: KEEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Kleene Expansion (Expansion Finite)" $ do
      matchEpsilon (Lower .
                    Lower . Lower $ Point Infinity :: KEEF) `shouldBe` Just (toZ sTwo)

    it "matches ε_0 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      matchEpsilon (Point Infinity :: KEEEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      matchEpsilon (Lower $ Point Infinity :: KEEEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      matchEpsilon (Lower . Lower $ Point Infinity :: KEEEF) `shouldBe` Just (toZ sTwo)

    it "matches ε_0 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEpsilon (Point . Lifted . Lower . Lower $ Point Infinity :: KEEKEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEpsilon (Point . Lifted . Lower . Lower . Lower $ Point Infinity :: KEEKEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEpsilon (Point . Lifted . Lower . Lower . Lower . Lower $ Point Infinity :: KEEKEF) `shouldBe` Just (toZ sTwo)

    it "matches ε_0 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      matchEpsilon (Point . Lower . Lower $ Point Infinity :: KEKEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      matchEpsilon (Point . Lower . Lower . Lower $ Point Infinity :: KEKEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      matchEpsilon (Point . Lower . Lower . Lower . Lower $ Point Infinity :: KEKEF) `shouldBe` Just (toZ sTwo)

    it "matches ε_0 :: Kleene (Kleene Expansion) Finite" $ do
      matchEpsilon (Lower . Point . Lower . Lower $ Point Infinity :: KKEF) `shouldBe` Just (toZ SZero)

    it "matches ε_1 :: Kleene (Kleene Expansion) Finite" $ do
      matchEpsilon (Lower . Point . Lower . Lower . Lower $ Point Infinity :: KKEF) `shouldBe` Just (toZ sOne)

    it "matches ε_2 :: Kleene (Kleene Expansion) Finite" $ do
      matchEpsilon (Lower . Point . Lower . Lower . Lower . Lower $ Point Infinity :: KKEF) `shouldBe` Just (toZ sTwo)

  describe "ε" $ do

    it "produces ε_0 :: Expansion (Expansion Finite)" $ do
      ε SZero `shouldBe` (Infinity :: EEF)

    it "produces ε_0 :: Expansion (Expansion (Expansion Finite))" $ do
      ε SZero `shouldBe` (Lifted Infinity :: EEEF)

    it "produces ε_1 :: Expansion (Expansion (Expansion Finite))" $ do
      ε sOne `shouldBe` (Infinity :: EEEF)

    it "produces ε_0 :: Expansion (Kleene Expansion Finite)" $ do
      ε SZero `shouldBe` (Lifted . Lower . Lower $ Point Infinity :: EKEF)

    it "produces ε_1 :: Expansion (Kleene Expansion Finite)" $ do
      ε sOne `shouldBe` (Lifted . Lower .
                               Lower . Lower $ Point Infinity :: EKEF)

    it "produces ε_2 :: Expansion (Kleene Expansion Finite)" $ do
      ε sTwo `shouldBe` (Lifted . Lower . Lower .
                               Lower . Lower $ Point Infinity :: EKEF)

    it "produces ε_0 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      ε SZero `shouldBe` (Lifted . Lifted .
                                Lower . Lower $ Point Infinity :: EEKEF)

    it "produces ε_1 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      ε sOne `shouldBe` (Lifted . Lifted . Lower .
                               Lower . Lower $ Point Infinity :: EEKEF)

    it "produces ε_2 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      ε sTwo `shouldBe` (Lifted . Lifted . Lower . Lower .
                               Lower . Lower $ Point Infinity :: EEKEF)

    it "produces ε_0 :: Kleene Expansion Finite" $ do
      ε SZero `shouldBe` (Lower . Lower $ Point Infinity :: KEF)

    it "produces ε_1 :: Kleene Expansion Finite" $ do
      ε sOne `shouldBe` (Lower .
                               Lower . Lower $ Point Infinity :: KEF)

    it "produces ε_2 :: Kleene Expansion Finite" $ do
      ε sTwo `shouldBe` (Lower . Lower .
                               Lower . Lower $ Point Infinity :: KEF)

    it "produces ε_0 :: Kleene Expansion (Expansion Finite)" $ do
      ε SZero `shouldBe` (Lower $ Point Infinity :: KEEF)

    it "produces ε_1 :: Kleene Expansion (Expansion Finite)" $ do
      ε sOne `shouldBe` (Lower . Lower $ Point Infinity :: KEEF)

    it "produces ε_2 :: Kleene Expansion (Expansion Finite)" $ do
      ε sTwo `shouldBe` (Lower . 
                               Lower . Lower $ Point Infinity :: KEEF)

    it "produces ε_0 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      ε SZero `shouldBe` (Point Infinity :: KEEEF)

    it "produces ε_1 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      ε sOne `shouldBe` (Lower $ Point Infinity :: KEEEF)

    it "produces ε_2 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      ε sTwo `shouldBe` (Lower . Lower $ Point Infinity :: KEEEF)

    it "produces ε_0 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      ε SZero `shouldBe` (Point . Lifted . Lower . Lower $ Point Infinity :: KEEKEF)

    it "produces ε_1 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      ε sOne `shouldBe` (Point . Lifted . Lower . Lower . Lower $ Point Infinity :: KEEKEF)

    it "produces ε_2 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      ε sTwo `shouldBe` (Point . Lifted . Lower . Lower . Lower . Lower $ Point Infinity :: KEEKEF)

    it "produces ε_0 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      ε SZero `shouldBe` (Point . Lower . Lower $ Point Infinity :: KEKEF)

    it "produces ε_1 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      ε sOne `shouldBe` (Point . Lower . Lower . Lower $ Point Infinity :: KEKEF)

    it "produces ε_2 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      ε sTwo `shouldBe` (Point . Lower . Lower . Lower . Lower $ Point Infinity :: KEKEF)

    it "produces ε_0 :: Kleene (Kleene Expansion) Finite" $ do
      ε SZero `shouldBe` (Lower . Point . Lower . Lower $ Point Infinity :: KKEF)

    it "produces ε_1 :: Kleene (Kleene Expansion) Finite" $ do
      ε sOne `shouldBe` (Lower . Point . Lower . Lower . Lower $ Point Infinity :: KKEF)

    it "produces ε_2 :: Kleene (Kleene Expansion) Finite" $ do
      ε sTwo `shouldBe` (Lower . Point . Lower . Lower . Lower . Lower $ Point Infinity :: KKEF)

  describe "Eps" $ do
    it "can be used as pattern for Expansion (Expansion Finite)" $ do
      let f :: EEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f Infinity `shouldBe` Just 0

    it "can be used as pattern for Expansion (Expansion (Expansion Finite))" $ do
      let f :: EEEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      catMaybes [ f Infinity, f (Lifted Infinity) ] `shouldBe` [1, 0]

    it "can be used as pattern for Expansion (Kleene Expansion Finite)" $ do
      let f :: EKEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f Infinity `shouldBe` Nothing
      f (Lifted .
         Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Expansion (Expansion (Kleene Expansion Finite))" $ do
      let f :: EEKEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f Infinity `shouldBe` Nothing
      f (Lifted Infinity) `shouldBe` Nothing
      f (Lifted . Lifted .
         Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene Expansion Finite" $ do
      let f :: KEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f (Point 0) `shouldBe` Nothing
      f (Lower $ Point Infinity) `shouldBe` Nothing
      f (Lower . Lower $ Point Infinity) `shouldBe` Just 0
      f (Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene Expansion (Expansion Finite)" $ do
      let f :: KEEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f (Point Infinity) `shouldBe` Nothing
      f (Lower $ Point Infinity) `shouldBe` Just 0
      f (Lower . Lower $ Point Infinity) `shouldBe` Just 1
      f (Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 5

    it "can be used as pattern for Kleene Expansion (Expansion (Expansion Finite))" $ do
      let f :: KEEEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f (Point Infinity) `shouldBe` Just 0
      f (Lower $ Point Infinity) `shouldBe` Just 1
      f (Lower . Lower $ Point Infinity) `shouldBe` Just 2
      f (Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 6

    it "can be used as pattern for Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      let f :: KEEKEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f (Point Infinity) `shouldBe` Nothing
      f (Point . Lifted .  Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene Expansion (Kleene Expansion Finite)" $ do
      let f :: KEKEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f (Point . Lower . Lower $ Point Infinity) `shouldBe` Just 0
      f (Point . Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

    it "can be used as pattern for Kleene (Kleene Expansion) Finite" $ do
      let f :: KKEF -> Maybe Int
          f (Eps i) = Just i
          f _ = Nothing
      f (Lower . Point .  Lower . Lower $ Point Infinity) `shouldBe` Just 0
      f (Lower . Point . Lower . Lower . Lower . Lower .  Lower . Lower $ Point Infinity) `shouldBe` Just 4

  describe "matchEps" $ do

    it "matches ε_0 :: Expansion (Expansion Finite)" $ do
      matchEps (Infinity :: EEF) `shouldBe` Just 0

    it "matches ε_0 :: Expansion (Expansion (Expansion Finite))" $ do
      matchEps (Lifted Infinity :: EEEF) `shouldBe` Just 0

    it "matches ε_1 :: Expansion (Expansion (Expansion Finite))" $ do
      matchEps (Infinity :: EEEF) `shouldBe` Just 1

    it "matches ε_0 :: Expansion (Kleene Expansion Finite)" $ do
      matchEps (Lifted . Lower . Lower $ Point Infinity :: EKEF) `shouldBe` Just 0

    it "matches ε_1 :: Expansion (Kleene Expansion Finite)" $ do
      matchEps (Lifted . Lower .
                    Lower . Lower $ Point Infinity :: EKEF) `shouldBe` Just 1

    it "matches ε_2 :: Expansion (Kleene Expansion Finite)" $ do
      matchEps (Lifted . Lower . Lower .
                    Lower . Lower $ Point Infinity :: EKEF) `shouldBe` Just 2

    it "matches ε_0 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEps (Lifted . Lifted .
                    Lower . Lower $ Point Infinity :: EEKEF) `shouldBe` Just 0

    it "matches ε_1 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEps (Lifted . Lifted . Lower .
                    Lower . Lower $ Point Infinity :: EEKEF) `shouldBe` Just 1

    it "matches ε_2 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEps (Lifted . Lifted . Lower . Lower .
                    Lower . Lower $ Point Infinity :: EEKEF) `shouldBe` Just 2

    it "matches ε_0 :: Kleene Expansion Finite" $ do
      matchEps (Lower . Lower $ Point Infinity :: KEF) `shouldBe` Just 0

    it "matches ε_1 :: Kleene Expansion Finite" $ do
      matchEps (Lower .
                    Lower . Lower $ Point Infinity :: KEF) `shouldBe` Just 1

    it "matches ε_2 :: Kleene Expansion Finite" $ do
      matchEps (Lower . Lower .
                    Lower . Lower $ Point Infinity :: KEF) `shouldBe` Just 2

    it "matches ε_0 :: Kleene Expansion (Expansion Finite)" $ do
      matchEps (Lower $ Point Infinity :: KEEF) `shouldBe` Just 0

    it "matches ε_1 :: Kleene Expansion (Expansion Finite)" $ do
      matchEps (Lower . Lower $ Point Infinity :: KEEF) `shouldBe` Just 1

    it "matches ε_2 :: Kleene Expansion (Expansion Finite)" $ do
      matchEps (Lower .
                    Lower . Lower $ Point Infinity :: KEEF) `shouldBe` Just 2

    it "matches ε_0 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      matchEps (Point Infinity :: KEEEF) `shouldBe` Just 0

    it "matches ε_1 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      matchEps (Lower $ Point Infinity :: KEEEF) `shouldBe` Just 1

    it "matches ε_2 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      matchEps (Lower . Lower $ Point Infinity :: KEEEF) `shouldBe` Just 2

    it "matches ε_0 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEps (Point . Lifted . Lower . Lower $ Point Infinity :: KEEKEF) `shouldBe` Just 0

    it "matches ε_1 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEps (Point . Lifted . Lower . Lower . Lower $ Point Infinity :: KEEKEF) `shouldBe` Just 1

    it "matches ε_2 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      matchEps (Point . Lifted . Lower . Lower . Lower . Lower $ Point Infinity :: KEEKEF) `shouldBe` Just 2

    it "matches ε_0 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      matchEps (Point . Lower . Lower $ Point Infinity :: KEKEF) `shouldBe` Just 0

    it "matches ε_1 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      matchEps (Point . Lower . Lower . Lower $ Point Infinity :: KEKEF) `shouldBe` Just 1

    it "matches ε_2 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      matchEps (Point . Lower . Lower . Lower . Lower $ Point Infinity :: KEKEF) `shouldBe` Just 2

    it "matches ε_0 :: Kleene (Kleene Expansion) Finite" $ do
      matchEps (Lower . Point . Lower . Lower $ Point Infinity :: KKEF) `shouldBe` Just 0

    it "matches ε_1 :: Kleene (Kleene Expansion) Finite" $ do
      matchEps (Lower . Point . Lower . Lower . Lower $ Point Infinity :: KKEF) `shouldBe` Just 1

    it "matches ε_2 :: Kleene (Kleene Expansion) Finite" $ do
      matchEps (Lower . Point . Lower . Lower . Lower . Lower $ Point Infinity :: KKEF) `shouldBe` Just 2

  describe "toEps" $ do

    it "produces ε_0 :: Expansion (Expansion Finite)" $ do
      toEps 0 `shouldBe` Just (Infinity :: EEF)

    it "can't produce ε_1 :: Expansion (Expansion Finite)" $ do
      toEps 1 `shouldBe` (Nothing :: Maybe EEF)

    it "can't produce ε_2 :: Expansion (Expansion Finite)" $ do
      toEps 2 `shouldBe` (Nothing :: Maybe EEF)

    it "produces ε_0 :: Expansion (Expansion (Expansion Finite))" $ do
      toEps 0 `shouldBe` Just (Lifted Infinity :: EEEF)

    it "produces ε_1 :: Expansion (Expansion (Expansion Finite))" $ do
      toEps 1 `shouldBe` Just (Infinity :: EEEF)

    it "can't produce ε_2 :: Expansion (Expansion Finite)" $ do
      toEps 2 `shouldBe` (Nothing :: Maybe EEEF)

    it "produces ε_0 :: Expansion (Kleene Expansion Finite)" $ do
      toEps 0 `shouldBe` Just (Lifted . Lower . Lower $ Point Infinity :: EKEF)

    it "produces ε_1 :: Expansion (Kleene Expansion Finite)" $ do
      toEps 1 `shouldBe` Just (Lifted . Lower .  Lower . Lower $ Point Infinity :: EKEF)

    it "produces ε_2 :: Expansion (Kleene Expansion Finite)" $ do
      toEps 2 `shouldBe` Just (Lifted . Lower . Lower .  Lower . Lower $ Point Infinity :: EKEF)

    it "produces ε_0 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      toEps 0 `shouldBe` Just (Lifted . Lifted .  Lower . Lower $ Point Infinity :: EEKEF)

    it "produces ε_1 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      toEps 1 `shouldBe` Just (Lifted . Lifted . Lower .  Lower . Lower $ Point Infinity :: EEKEF)

    it "produces ε_2 :: Expansion (Expansion (Kleene Expansion Finite))" $ do
      toEps 2 `shouldBe` Just (Lifted . Lifted . Lower . Lower .  Lower . Lower $ Point Infinity :: EEKEF)

    it "produces ε_0 :: Kleene Expansion Finite" $ do
      toEps 0 `shouldBe` Just (Lower . Lower $ Point Infinity :: KEF)

    it "produces ε_1 :: Kleene Expansion Finite" $ do
      toEps 1 `shouldBe` Just (Lower .  Lower . Lower $ Point Infinity :: KEF)

    it "produces ε_2 :: Kleene Expansion Finite" $ do
      toEps 2 `shouldBe` Just (Lower . Lower .  Lower . Lower $ Point Infinity :: KEF)

    it "produces ε_0 :: Kleene Expansion (Expansion Finite)" $ do
      toEps 0 `shouldBe` Just (Lower $ Point Infinity :: KEEF)

    it "produces ε_1 :: Kleene Expansion (Expansion Finite)" $ do
      toEps 1 `shouldBe` Just (Lower . Lower $ Point Infinity :: KEEF)

    it "produces ε_2 :: Kleene Expansion (Expansion Finite)" $ do
      toEps 2 `shouldBe` Just (Lower .  Lower . Lower $ Point Infinity :: KEEF)

    it "produces ε_0 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      toEps 0 `shouldBe` Just (Point Infinity :: KEEEF)

    it "produces ε_1 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      toEps 1 `shouldBe` Just (Lower $ Point Infinity :: KEEEF)

    it "produces ε_2 :: Kleene Expansion (Expansion (Expansion Finite))" $ do
      toEps 2 `shouldBe` Just (Lower . Lower $ Point Infinity :: KEEEF)

    it "produces ε_0 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      toEps 0 `shouldBe` Just (Point . Lifted . Lower . Lower $ Point Infinity :: KEEKEF)

    it "produces ε_1 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      toEps 1 `shouldBe` Just (Point . Lifted . Lower . Lower . Lower $ Point Infinity :: KEEKEF)

    it "produces ε_2 :: Kleene Expansion (Expansion (Kleene Expansion Finite))" $ do
      toEps 2 `shouldBe` Just (Point . Lifted . Lower . Lower . Lower . Lower $ Point Infinity :: KEEKEF)

    it "produces ε_0 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      toEps 0 `shouldBe` Just (Point . Lower . Lower $ Point Infinity :: KEKEF)

    it "produces ε_1 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      toEps 1 `shouldBe` Just (Point . Lower . Lower . Lower $ Point Infinity :: KEKEF)

    it "produces ε_2 :: Kleene Expansion (Kleene Expansion Finite)" $ do
      toEps 2 `shouldBe` Just (Point . Lower . Lower . Lower . Lower $ Point Infinity :: KEKEF)

    it "produces ε_0 :: Kleene (Kleene Expansion) Finite" $ do
      toEps 0 `shouldBe` Just (Lower . Point . Lower . Lower $ Point Infinity :: KKEF)

    it "produces ε_1 :: Kleene (Kleene Expansion) Finite" $ do
      toEps 1 `shouldBe` Just (Lower . Point . Lower . Lower . Lower $ Point Infinity :: KKEF)

    it "produces ε_2 :: Kleene (Kleene Expansion) Finite" $ do
      toEps 2 `shouldBe` Just (Lower . Point . Lower . Lower . Lower . Lower $ Point Infinity :: KKEF)
#endif

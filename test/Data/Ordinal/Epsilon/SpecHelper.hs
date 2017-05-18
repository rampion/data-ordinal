{-# LANGUAGE DataKinds #-}
module Data.Ordinal.Epsilon.SpecHelper where
import Data.Ordinal.Expansion
import Data.Ordinal.Finite
import Data.Ordinal.Kleene
import Data.Ordinal.Epsilon

type EEF = Expansion (Expansion Finite)
type EEEF = Expansion EEF
type EKEF = Expansion KEF
type EEKEF = Expansion EKEF
type KEF = Kleene Expansion Finite
type KEEF = Kleene Expansion (Expansion Finite)
type KEEEF = Kleene Expansion (Expansion (Expansion Finite))
type KEEKEF = Kleene Expansion (Expansion (Kleene Expansion Finite))
type KEKEF = Kleene Expansion (Kleene Expansion Finite)
type KKEF = Kleene (Kleene Expansion) Finite

type PlusTwo n = 'NSucc ('NSucc n)
sPlusTwo :: SNat n -> SNat (PlusTwo n)
sPlusTwo = SSucc . SSucc

type PlusFour n = PlusTwo (PlusTwo n)
sPlusFour :: SNat n -> SNat (PlusFour n)
sPlusFour = sPlusTwo . sPlusTwo

type PlusEight n = PlusFour (PlusFour n)
sPlusEight :: SNat n -> SNat (PlusEight n)
sPlusEight = sPlusFour . sPlusFour

type PlusSixteen n = PlusEight (PlusEight n)
sPlusSixteen :: SNat n -> SNat (PlusSixteen n)
sPlusSixteen = sPlusEight . sPlusEight

type NOne = 'NSucc 'NZero
sOne :: SNat NOne
sOne = SSucc SZero

type NTwo = PlusTwo 'NZero
sTwo :: SNat NTwo
sTwo = sPlusTwo SZero

type NEight = PlusEight 'NZero
sEight :: SNat NEight
sEight = sPlusEight SZero

type NSixteen = PlusSixteen 'NZero
sSixteen :: SNat NSixteen
sSixteen = sPlusSixteen SZero

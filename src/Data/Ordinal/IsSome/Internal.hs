{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Data.Ordinal.IsSome.Internal where
import GHC.Exts (Constraint)

data IsSome a t b where
  Reflexive :: a `IsSome` t `Of` a
  Inductive :: a `IsSome` t `Of` b -> t a `IsSome` t `Of` b

type Of (f :: j -> k) (b :: j) = f b
infixr 0 `Of`

total :: x `IsSome` t `Of` a -> y `IsSome` t `Of` a -> Either (y `IsSome` t `Of` x) (x `IsSome` t `Of` y)
total Reflexive ay = Left ay
total ax Reflexive = Right ax
total (Inductive ax) (Inductive ay) = case total ax ay of
  Left xy -> Left $ closed xy
  Right yx -> Right $ closed yx

closed :: a `IsSome` t `Of` b -> t a `IsSome` t `Of` t b
closed Reflexive = Reflexive
closed (Inductive pf) = Inductive (closed pf)

fold :: (forall x. x -> t x) -> b -> a `IsSome` t `Of` b  -> a
fold _ b Reflexive = b
fold t b (Inductive pf) = t (fold t b pf)

type family All (cs :: [* -> Constraint]) (a :: *) :: Constraint where
  All '[] a = ()
  All (c ': cs) a = (c a, All cs a)

data Satisfied (cs :: [* -> Constraint]) (a :: *) where
  Satisfied :: All cs a => Satisfied cs a

induction :: All cs b => (forall x. Satisfied cs x -> Satisfied cs (t x)) -> a `IsSome` t `Of` b -> Satisfied cs a
induction _ Reflexive = Satisfied
induction f (Inductive pf) = f (induction f pf)

implies :: Satisfied cs a -> (All cs a => x) -> x
Satisfied `implies` x = x

class Wrap t where 
  type Base t b :: Constraint
  wrap :: Base t b => a `IsSome` t `Of` t b -> b -> t a

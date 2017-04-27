{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Ordinal.IsSome 
  ( IsSome(..)
  , wrap
  , I.Of
  ) where

import qualified Data.Ordinal.IsSome.Internal as I

class IsSome a t b where
  isSome :: a `I.IsSome` t `I.Of` b

instance IsSome a t a where
  isSome = I.Reflexive

instance IsSome a t b => IsSome (t a) t b where
  isSome = I.Inductive isSome

wrap :: (I.Wrap t, I.Base t b, a `IsSome` t `I.Of` b) => b -> t a
wrap b = I.wrap isSome b

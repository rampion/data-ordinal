{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Data.Ordinal.Notation where
import Control.Arrow (first)
import Data.Typeable

type To x = Maybe (Int, TyCon) -> Maybe ([Int], TyCon, TypeRep) -> TypeRep -> x

notation :: Typeable a => proxy a -> Int -> ShowS
notation = \proxy -> parse (typeRep proxy) getName where

  parse :: TypeRep -> To x -> x
  parse r@(splitTyConApp -> (c,rs)) f = case (show c, rs) of
    ("Expansion", [a]) -> 
      parse a $ \n -> f $! incr c n
    ("Kleene", [t,b]) ->
      case parse1 0 t of 
        --- (K{n}E)(K{m < n}E)+F ~ (K{n}E)F
        Just (k,r') -> parse b . const $ f Nothing . push c r' k
        Nothing -> f Nothing Nothing (mkTyConApp c [t, parse b unroll])
    _ -> f Nothing Nothing r

  -- parse1 0 r = Just (n,_) when r ~ (K{n}E)
  --            = Nothing otherwise
  parse1 :: Int -> TypeRep -> Maybe (Int, TypeRep)
  parse1 k r = case first show (splitTyConApp r) of
    ("Expansion", []) -> Just (k,r)
    ("Kleene", [t]) -> (parse1 $! k + 1) t
    _  -> Nothing

  incr c Nothing = Just (1,c)
  incr c (Just (n,_)) = Just . (,c) $! n + 1

  push c r k Nothing = Just ([k],c,r)
  push c r k (Just (ks,_,_)) = Just (k : dropWhile (<k) ks, c, r)

  unroll :: To TypeRep
  unroll mn mks = 
    maybe id (\(n,c) -> apply n c) mn .
    maybe id (\(ks,c,c') -> apply1 ks c c') mks

  apply n c b = foldr (\_ b' -> mkTyConApp c [b']) b $ replicate n c
  apply1 ks c r b = foldr (\k b' -> mkTyConApp c [apply k c r, b']) b ks

  getName :: To (Int -> ShowS)
  getName mn mks b = case (show b == "Finite", maybe 0 fst mn, maybe [] fst3 mks) of
    (True, 0, []) -> const $ showString "ω"
    (True, n, []) -> const . showString $ "ε_" ++ show (n - 1)
    (True, 0, [0]) -> const $ showString "ω_ω"
    _              -> 
      let name = "Infinity @" ++ showsPrec 10 (unroll mn mks b) ""
      in \p -> showParen (p >= 10) $ showString name

  fst3 :: (a,b,c) -> a
  fst3 (a,_,_) = a

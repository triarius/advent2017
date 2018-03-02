{-# LANGUAGE RoleAnnotations,
             DataKinds,
             Rank2Types,
             FlexibleInstances,
             KindSignatures,
             ScopedTypeVariables,
             TypeApplications #-}

module Advent.Lib.Symmetric
    ( Sym (..)
    , mkSym
    , swap
    , cycleDecomp
    , order
    , int2Fin
    , fin2Int
    ) where

import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Finite
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import           GHC.TypeNats

type role Sym nominal

newtype Sym (n :: Nat) = Sym (V.Vector Int)
    deriving (Eq, Ord, Read, Show)

instance forall n. KnownNat n => Semigroup (Sym n) where
    Sym x <> Sym y = Sym (V.backpermute y x)
    sconcat (x :| xs) = foldl' (<>) x xs

instance forall n. KnownNat n => Monoid (Sym n) where
    mempty  = mkSym id
    mappend = (<>)
    mconcat = foldl' mappend mempty

mkSym :: forall n. KnownNat n => (Finite n -> Finite n) -> Sym n
mkSym f = Sym . V.generate l $ f'
  where l  = fromIntegral . natVal $ Proxy @n
        f' = fin2Int . f . unsafeInt2Fin

unsafeInt2Fin :: KnownNat n => Int -> Finite n
unsafeInt2Fin = finite . toInteger

int2Fin :: forall n. KnownNat n => Int -> Finite n
int2Fin = finite . toInteger . (`mod` l)
  where l  = fromIntegral . natVal $ Proxy @n

fin2Int :: KnownNat n => Finite n -> Int
fin2Int = fromInteger . getFinite

swap :: KnownNat n => Finite n -> Finite n -> Sym n
swap i j = mkSym $ transposition i j

transposition :: KnownNat n => Finite n -> Finite n -> Finite n -> Finite n
transposition p q x
  | x == p    = q
  | x == q    = p
  | otherwise = x

cycleDecomp :: forall n. KnownNat n => Sym n -> [[Finite n]]
cycleDecomp g = snd $ cycleDecomp' (S.fromList $ take l [0..], [])
  where l = fromIntegral . natVal $ Proxy @n
        cycleDecomp' :: KnownNat n
                     => (S.Set (Finite n), [[Finite n]])
                     -> (S.Set (Finite n), [[Finite n]])
        cycleDecomp' (set, cs)
          | S.null set = (S.empty, cs)
          | otherwise = cycleDecomp' (set', c : cs)
          where (set', c) = cycleFrom g (S.elemAt 0 set) (set, [])

applyPerm :: KnownNat n => Sym n -> Finite n -> Finite n
applyPerm (Sym v) = unsafeInt2Fin . (V.!) v . fin2Int

-- cycleFrom g x (set, cps) = (set', cps') where g is a permutation, x is the start of the cycle,
-- set is the remaining elements that the cycle may visit and cps is an accumulator that holds
-- where the cycle has already visted. set' = set (\\) x and cps' = cps ++ g(x)
cycleFrom :: KnownNat n
          => Sym n
          -> Finite n
          -> (S.Set (Finite n), [Finite n])
          -> (S.Set (Finite n), [Finite n])
cycleFrom g x (set, cps) = if S.member x set then (set', cps') else (set, cps)
  where (set', cps') = cycleFrom g (applyPerm g x) (S.delete x set, x : cps)

order :: KnownNat n => Sym n -> Int
order g = foldr lcm 1 cycleLengths
  where cycleLengths = map length . cycleDecomp $ g

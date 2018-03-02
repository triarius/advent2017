module Advent.Prob17 (solution) where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Sequence (Seq(..), (|>), (<|), singleton)

data CList a = CList (Seq a) (Seq a) deriving Show

chead :: CList a -> a
chead (CList Empty _) = error "invalid cyclic list (empty front)"
chead (CList (x :<| _) _) = x

ctail :: CList a -> CList a
ctail (CList Empty _)          = error "invalid cyclic list (empty front)"
ctail (CList (x :<| Empty) ys) = CList (ys |> x) Empty
ctail (CList (x :<| xs) ys)    = CList xs (ys |> x)

step :: Int -> Int -> CList Int -> CList Int
step 0 val (CList (x :<| xs) ys) = CList (val <| xs) (ys |> x)
step n val cxs                   = step (n - 1) val (ctail cxs)

after0 :: Int -> Int -> (Int, Int) -> (Int, Int)
after0 size ins (cur, succ0) = (cur' + 1, if cur' == 0 then ins else succ0)
  where cur' = (cur + size) `rem` ins

part1 :: Int -> Int
part1 input = chead . ctail . foldl'  (flip (step input)) (CList (singleton 0) Empty) $ [1..2017]

part2 :: Int -> Int
part2 input = snd . foldl' (flip (after0 input)) (0, 0) $ [1..50000000]

solution :: String -> (Int, Int)
solution = (part1 &&& part2) . read

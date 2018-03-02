module Advent.Prob15 (solution) where

import Advent.Lib.ParseIO
import Control.Monad
import Data.Bifunctor
import Data.Bits

multA :: Int
multA = 16807

multB :: Int
multB = 48271

startA :: Int
startA = 722

startB :: Int
startB = 354

mask :: Int
mask = 0xFFFF

divisor :: Int
divisor = 2147483647

gen :: Int -> Int -> Int
gen last mult = (last * mult) `rem` divisor

list :: Int -> Int -> [Int]
list mult = iterate (gen mult)

match :: Int -> Int -> Bool
match x y = mask .&. x == mask .&. y

solution :: String -> (Int, Int)
solution _ = join bimap (length . filter id . uncurry (zipWith match)) (lists1, lists2)
  where
    lists  = join bimap (uncurry list) ((multA, startA), (multB, startB))
    lists1 = join bimap (take (40*10^6)) lists
    lists2 = join bimap (take (5*10^6))
           . first (filter ((==0) . (.&.3)))
           . second (filter ((==0) . (.&.7)))
           $ lists

module Advent.Prob01 (solution) where

import Control.Arrow ((&&&))
import Data.Char (isSpace)

clean :: String -> [String]
clean = map pure . filter (not . isSpace)

matchesOffset :: Int -> [Int] -> [Int]
matchesOffset o xs = map fst . filter snd . zipWith (\x y -> (x, x == y)) xs . drop o . cycle $ xs

part1 :: [Int] -> Int
part1 = sum . matchesOffset 1

part2 :: [Int] -> Int
part2 ints = sum . matchesOffset (length ints `quot` 2) $ ints

solution :: String -> (Int, Int)
solution input = let ints = map read . clean $ input
                  in (part1 &&& part2) ints

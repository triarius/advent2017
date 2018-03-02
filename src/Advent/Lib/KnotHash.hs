module Advent.Lib.KnotHash (hashRound, knotHash) where

import Data.Bits
import Data.Char (ord, intToDigit)
import Data.List (unfoldr)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)

pinchTwist :: Int -> (Int, Int, [Int]) -> Int -> (Int, Int, [Int])
pinchTwist totalLen (start, skip, xs) len = (start', skip', xs')
  where
    skip'  = skip + 1
    start' = (start - len - skip) `mod` totalLen
    (toRev, rest) = splitAt len xs
    pinched = reverse toRev ++ rest
    xs'     = take totalLen . drop ((len + skip) `rem` totalLen) . cycle $ pinched

hashRound :: Int -> [Int] -> (Int, Int, [Int]) -> (Int, Int, [Int])
hashRound len input (start, skip, xs) = foldl (pinchTwist len) (start, skip, xs) input

knotHash :: Int -> String -> String
knotHash len input = ints2Hash rotated
  where
    salt = [17, 31, 73, 47, 23]
    (start, _, xs) = (!! 64) . iterate (hashRound len $ map ord input ++ salt) $ (0, 0, [0..len-1])
    rotated = take len . drop start . cycle $ xs

ints2Hash :: [Int] -> String
ints2Hash = concatMap (pad . map intToDigit . hexDigits . foldr1 xor) . chunksOf 16
  where pad []  = "00"
        pad [x] = '0' : [x]
        pad xs  = xs

hexDigits :: Int -> [Int]
hexDigits = reverse . unfoldr f
  where
    f :: Int -> Maybe (Int, Int)
    f 0 = Nothing
    f x = Just . swap . (`quotRem` 16) $ x

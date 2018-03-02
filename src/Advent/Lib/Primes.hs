module Advent.Lib.Primes (isPrime, primes) where

import qualified Data.PQueue.Prio.Min as PQ

primes :: [Integer]
primes = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)

wheel2357 :: [Integer]
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:
            6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spin :: [Integer] -> Integer -> [Integer]
spin [] _ = []
spin (x:xs) n = n : spin xs (n + x)

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve' xs (insertPrime p xs PQ.empty)

insertPrime :: Integer
            -> [Integer]
            -> PQ.MinPQueue Integer [Integer]
            -> PQ.MinPQueue Integer [Integer]
insertPrime p ys = PQ.insert (p*p) (map (*p) ys)

sieve' :: [Integer] -> PQ.MinPQueue Integer [Integer] -> [Integer]
sieve' [] _ = []
sieve' (z:zs) table
  | nextComposite <= z = sieve' zs (adjust table)
  | otherwise = z : sieve' zs (insertPrime z zs table)
    where
        nextComposite = (fst . PQ.findMin) table
        adjust :: PQ.MinPQueue Integer [Integer] -> PQ.MinPQueue Integer [Integer]
        adjust table'
          | n <= z = adjust $ PQ.insert n' ns (PQ.deleteMin table')
          | otherwise = table'
            where (n, n':ns) = PQ.findMin table'

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime p = not . any ((0==) . (p' `rem`)) . takeWhile (<= isqrt p') $ primes
    where p' = abs p

(^!) :: Num a => a -> Integer -> a
(^!) x n = x ^ n

isqrt :: Integer -> Integer
isqrt 0 = 0
isqrt 1 = 1
isqrt n = (head . dropWhile isNotRoot . iterate newtonRapson) x0
    where
        isNotRoot x = x^!2 > n
        newtonRapson x = (x^!2 + n) `div` (2*x)
        x0 = n `div` 2

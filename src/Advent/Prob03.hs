{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Prob03 (solution) where

import           Control.Monad.Loops      (dropWhileM)
import           Control.Monad.ST         (runST, ST)
import           Data.Maybe               (fromMaybe)
import           Data.Hashable            (Hashable)
import           Data.HashTable.ST.Cuckoo (HashTable)
import qualified Data.HashTable.Class     as H


(^!) :: Num a => a -> Int -> a
x ^! y = x ^ y

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

coords :: Int -> (Int, Int)
coords 1 = (0,0)
coords n = let layer  = csqrt n `quot` 2
               period = 2 * layer
               start  = (period - 1)^!2 + 1
               (q, r) = (n - start) `quotRem` period
            in (!! q) . iterate rotateACW $ (layer, r - layer + 1)

rotateACW :: (Int, Int) -> (Int, Int)
rotateACW (x, y) = (-y, x)

csqrt :: Integral a => a -> a
csqrt 0 = 0
csqrt 1 = 1
csqrt 2 = 2
csqrt n = csqrt' 0 n
  where csqrt' lo hi | m^!2 >= n && (m-1)^!2 < n = m
                     | m^!2 >= n                 = csqrt' lo m
                     | otherwise                 = csqrt' m hi
          where m = (lo + hi) `quot` 2

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y-1), (x,y+1), (x+1,y-1), (x+1,y), (x+1,y+1)]

calcStress :: Int -> Int
calcStress input = runST $ do
    ht <- H.new
    H.insert ht (0,0) 1
    (x:_) <- dropWhileM (return . (<=input)) =<< mapM (fillSquare ht) [2..100]
    return x

fillSquare :: HashTable s (Int, Int) Int -> Int -> ST s Int
fillSquare ht pos = do
    let c = coords pos
    stresses <- mapM (lookupDef ht 0) $ neighbours c
    let stress = sum stresses
    H.insert ht c stress
    return stress

lookupDef :: (Eq k, Hashable k) => HashTable s k v -> v -> k -> ST s v
lookupDef ht def key = fromMaybe def <$> H.lookup ht key

solution :: String -> (Int, Int)
solution input = (manhattan . coords $ read input, calcStress $ read input)

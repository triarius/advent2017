{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Advent.Prob14 (solution) where

import           Advent.Lib.ParseIO
import           Advent.Lib.KnotHash        (knotHash)
import           Advent.Lib.UnionFind      (UnionFind(..), MUnionFind(..))
import qualified Advent.Lib.UnionFind      as U
import           Control.Arrow
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.Unboxed
import qualified Data.Array                 as A
import           Data.Array.MArray          (MArray)
import qualified Data.Array.MArray          as M
import           Data.Array.ST
import           Data.Char                  (digitToInt, isSpace, intToDigit)
import           Data.Ix
import           Data.List                  (foldl')
import           Data.List.Split            (chunksOf)
import           Data.Maybe                 (mapMaybe)
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Text.Megaparsec            (parseMaybe)
import           Text.Megaparsec.Char.Lexer as L
import           Numeric                    (showIntAtBase)

dim :: Int
dim = 128

integerHashes :: String -> [Integer]
integerHashes input = mapMaybe (parseMaybe hexadecimal . knotHash 256 . append) [0..dim-1]
  where
    append :: Int -> String
    append = ((input++"-")++) . show
    hexadecimal :: Parser Integer
    hexadecimal = lexeme L.hexadecimal
    lexeme = L.lexeme (pure ())

mkDisk :: [Integer] -> String
mkDisk = concatMap (pad 128 '0' . flip (showIntAtBase 2 intToDigit) "")
  where
    pad :: Int -> Char -> String -> String
    pad n c str = replicate (n - length str) c ++ str

population :: String -> Int
population = foldl' (\acc x -> case x of
                                 '1' -> acc + 1
                                 _   -> acc) 0

mkArray :: String -> Array (Int, Int) Bool
mkArray = A.listArray ((0, 0), ((+(-1)) &&& (+(-1))) dim) . map toBool
  where toBool '1' = True
        toBool _   = False

process :: Array (Int, Int) Bool -> UnionFind (Int, Int)
process disk = runST $ do
    uf <- U.new (bounds disk) :: ST s (MUnionFind (STArray s) (Int, Int) (ST s))
    uf' <- foldM (processSquare disk) uf (A.indices disk)
    U.flatten uf'
    U.freeze uf'

processSquare :: (MArray a (Int, Int) m, MArray a Int m)
              => Array (Int, Int) Bool
              -> MUnionFind a (Int, Int) m
              -> (Int, Int)
              -> m (MUnionFind a (Int, Int) m)
processSquare disk uf = foldM processNbhrs uf . toUnion
  where
    processNbhrs u pair = uncurry (U.union u) pair >> pure u
    toUnion p
      | disk A.! p = map (p,) . filter (disk A.!) . prevNbhrs $ p
      | otherwise  = []
    prevNbhrs (x, y)
      | x > 0 && y > 0  = [(x - 1, y), (x, y - 1)]
      | x > 0           = [(x - 1, y)]
      | y > 0           = [(x, y - 1)]
      | otherwise       = []

regions :: Array (Int, Int) Bool -> Int
regions disk = uncurry (-) . (pi0 . process &&& length . filter (not . (disk A.!)) . indices) $ disk
  where pi0 = length . filter (uncurry (==)) . A.assocs . U.parents


solution :: String -> (Int, Int)
solution input = let disk = mkDisk . integerHashes . filter (not . isSpace) $ input
                  in (population &&& regions . mkArray) disk

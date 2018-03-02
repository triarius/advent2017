{-# LANGUAGE TupleSections #-}

module Advent.Prob02 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((&&&))
import           Data.List (sort)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

sheet :: Parser [[Int]]
sheet = row `endBy` eol
  where row = L.decimal `sepEndBy` (char ' ' <|> char '\t')

checksum :: [[Int]] -> Int
checksum = sum . map range
  where range xs = maximum xs - minimum xs

checksum2 :: [[Int]] -> Int
checksum2 = sum . map ( sum
                      . map fst
                      . filter ((==0) . snd)
                      . map (uncurry $ flip quotRem)
                      . comb2
                      . sort)

comb2 :: [a] -> [(a,a)]
comb2 [] = []
comb2 (x:xs) = map (x,) xs ++ comb2 xs

solution :: String -> (Int, Int)
solution input = let Just ss = parseMaybe sheet input
                  in (checksum &&& checksum2) ss

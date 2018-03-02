module Advent.Prob21 (solution) where

import           Advent.Lib.Misc
import           Advent.Lib.ParseIO
import           Data.List (transpose)
import           Data.List.Split (chunksOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Text.Megaparsec
import           Text.Megaparsec.Char


type Pic = [String]
type Rules = Map Pic Pic

rules :: Parser Rules
rules = Map.fromList <$> rule `endBy` eol
  where rule = (,) <$> picture <* string " => " <*> picture
        picture = row `sepBy` char '/'
        row =  many (oneOf ['.', '#'])

-- the initial picture
initial :: Pic
initial = [".#.","..#","###"]

rCW :: Pic -> Pic
rCW = reverse . transpose

d8 :: [Pic -> Pic]
d8 = concatMap (take 4 . iterate (rCW .)) [id, reverse]

canonical :: Rules -> Pic -> Pic
canonical rs p = fromJust . head . filter (/=Nothing) . map (`Map.lookup` rs) $ d8 <*> [p]

blocks :: [[a]] -> [[[[a]]]]
blocks xs = map transpose . chunksOf n . map (chunksOf n) $ xs
  where
    n | even $ length xs = 2
      | otherwise        = 3

unblocks :: [[[[a]]]] -> [[a]]
unblocks = concatMap (map concat . transpose)

mapBlocks :: ([[a]] -> [[a]]) -> [[a]] -> [[a]]
mapBlocks f = unblocks . map (map f) . blocks

countOn :: Pic -> Int
countOn = length . concatMap (filter (=='#'))

partSolution :: Rules -> Int -> Int
partSolution rs n = countOn . (!! n) . iterate (mapBlocks (canonical rs)) $ initial

solution :: String -> (Int, Int)
solution input = let Just rs = parseMaybe rules input
                  in pairMap (partSolution rs) (5, 18)

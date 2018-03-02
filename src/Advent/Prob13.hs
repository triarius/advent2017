module Advent.Prob13 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((&&&))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


firewall :: Parser [(Int, Int)]
firewall = layer `endBy` eol
  where
    layer = (,) <$> posInt <* symbol ":" <*> L.decimal
    lexeme = L.lexeme sc
    posInt = lexeme L.decimal
    symbol = L.symbol sc
    sc = skipMany (char ' ')

position :: Int -> Int -> Int
position time range = if cycleDist < range then cycleDist else 2 * range - cycleDist - 2
  where cycleDist = time `rem` period range

period :: Int -> Int
period range = range * 2 - 2

severity :: Int -> [(Int, Int)] -> Int
severity delay = foldr combiner 0
  where
    combiner :: (Int, Int) -> Int -> Int
    combiner (depth, range) acc = if caught then acc + (depth * range) else acc
      where caught = position (depth + delay) range == 0

positions :: Int -> [(Int, Int)] -> [(Int, Int)]
positions delay = map (\(depth, range) -> (depth, position (depth + delay) range))

findDelayOfFreedom :: [(Int, Int)] -> [(Int, [(Int, Int)])]
findDelayOfFreedom scanners = map (\x -> (x, positions x scanners)) [0..]

answer :: [(Int, Int)] -> Int
answer scanners = fst . head $
    filter (\(_, xs) -> all (\(_, x) -> x /= 0) xs) $
    findDelayOfFreedom scanners

solution :: String -> (Int, Int)
solution input = let Just scanners = parseMaybe firewall input
                  in (severity 0 &&& answer) scanners

{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Prob11 (solution) where

import           Advent.Lib.ParseIO
import qualified Data.Matrix as M
import           Data.Either (rights)
import           Text.Megaparsec
import           Text.Megaparsec.Char


type Direction = M.Matrix Double

directions :: Parser [Direction]
directions = direction `endBy` eol
  where
    direction = M.fromList 2 1 <$> choice [ string "n"  >> pure [1, 0]
                                          , string "ne" >> pure [0, 1]
                                          , string "se" >> pure [-1, 1]
                                          , string "s"  >> pure [-1, 0]
                                          , string "sw" >> pure [0, -1]
                                          , string "nw" >> pure [1, -1] ]

minSteps :: (RealFrac a, Integral b) => M.Matrix a -> b
minSteps v = round . minimum . map (numSteps . (`M.multStd` v)) $ transitions

numSteps :: Fractional a => M.Matrix a -> a
numSteps = foldr ((+) . abs) 0

transitions :: (Fractional a, Eq a) => [M.Matrix a]
transitions = rights [Right $ M.identity 2,
                      M.inverse . M.transpose . M.fromList 2 2 $ [1, 0, 1, -1],
                      M.inverse . M.transpose . M.fromList 2 2 $ [1, -1, 0, 1]]

destDiam :: (RealFrac a, Integral b) => [M.Matrix a] -> (M.Matrix a, b)
destDiam = foldl combine (M.zero 2 1, 0)
  where
    combine (oldPos, oldMax) x = let newPos = M.elementwise (+) oldPos x
                                  in (newPos, maxWith minSteps oldMax newPos)
    maxWith f x y = let fy = f y in if x < fy then fy else x

solution :: String -> (Int, Int)
solution input = let Just ds      = parseMaybe directions input
                     (dest, diam) = destDiam ds
                  in (minSteps dest, diam)

module Advent.Prob22 (solution) where

import           Advent.Lib.ParseIO
import           Data.List (genericLength)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Math.NumberTheory.GaussianIntegers
import           Text.Megaparsec
import           Text.Megaparsec.Char

type CarrierState = (GaussianInteger, GaussianInteger)

data NodeState = Clean | Weakended | Infected | Flagged deriving Eq

instance Ord GaussianInteger where
  (x1 :+ y1) <= (x2 :+ y2) = if x1 == x2 then y1 <= y2 else x1 <= x2

gridP :: Parser [String]
gridP = line `endBy` eol
  where line = many (oneOf ['.','#'])

createMap :: [String] -> Map GaussianInteger NodeState
createMap xss = snd $ foldr foldRows (0, Map.empty) xss
  where
    foldRows xs (i, m) = (i + 1, snd . foldl f (0, m) $ xs)
      where f (j, m') x = (j + 1, if x == '#' then Map.insert (j :+ i) Infected m' else m')

findStart :: [String] -> CarrierState
findStart strs = (mid :+ mid, ι)
  where mid = genericLength strs `div` 2

burst :: (CarrierState, Map GaussianInteger NodeState, Int)
      -> (CarrierState, Map GaussianInteger NodeState, Int)
burst ((z, dir), m, n) = case Map.findWithDefault Clean z m of
                           Infected -> let dir' = dir*(-ι)
                                        in ((z + dir', dir'), Map.insert z Clean m, n)
                           Clean    -> let dir' = dir*ι
                                        in ((z + dir', dir'), Map.insert z Infected m, n+1)
                           _        -> error "wrong part"

burst2 :: (CarrierState, Map GaussianInteger NodeState, Int)
       -> (CarrierState, Map GaussianInteger NodeState, Int)
burst2 ((z, dir), m, n) = case Map.findWithDefault Clean z m of
                            Clean     -> let dir' = dir*ι
                                          in ((z + dir', dir'), Map.insert z Weakended m, n)
                            Weakended -> ((z + dir, dir), Map.insert z Infected m, n+1)
                            Infected  -> let dir' = dir*(-ι)
                                          in ((z + dir', dir'), Map.insert z Flagged m, n)
                            Flagged   -> let dir' = -dir
                                          in ((z + dir', dir'), Map.insert z Clean m, n)

solution :: String -> (Int, Int)
solution input = let Just strs = parseMaybe gridP input
                     grid      = createMap strs
                     start     = findStart strs
                     (_, _, n1) = (!! 10000) . iterate burst $ (start, grid, 0)
                     (_, _, n2) = (!! 10000000) . iterate burst2 $ (start, grid, 0)
                  in (n1, n2)

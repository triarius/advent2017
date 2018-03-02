{-# LANGUAGE MultiWayIf #-}

module Advent.Prob24 (solution) where

import           Advent.Lib.ParseIO
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Cmpnt = C Int Int Int deriving (Eq, Ord)
instance Show Cmpnt where
  show (C x y _) = show x ++ "/" ++ show y

components :: Parser [Cmpnt]
components = zipWith (flip . uncurry $ C) [1..] <$> component `endBy` eol
  where component = (,) <$> L.decimal <* char '/' <*> L.decimal

cmpnt2Set :: Cmpnt -> IntSet
cmpnt2Set (C x y _) = IS.fromList [x, y]

hasEnd :: Int -> Cmpnt -> Bool
hasEnd end c = IS.member end $ cmpnt2Set c

otherEnd :: Int -> Cmpnt -> Int
otherEnd x (C y1 y2 _)
  | x == y1   = y2
  | otherwise = y1

nextCmpnts :: Int -> Set Cmpnt -> [Cmpnt] -> [Cmpnt]
nextCmpnts fr mrkd = filter ( and
                            . ([hasEnd fr, not . (`S.member` mrkd)] <*>)
                            . pure )

search :: [Cmpnt]         -- list of components
       -> Int             -- number of pins on free end
       -> Int             -- strength of current bridge
       -> Int             -- length of current bridge
       -> Set Cmpnt       -- components in current bridge
       -> Cmpnt           -- current end component
       -> (Int, Int, Int) -- current state
       -> (Int, Int, Int) -- output state
search cs fr s l mrkd c st@(maxS, maxL, strL)
  | S.member c mrkd = st
  | otherwise       = foldr searchNext (maxS', maxL', strL') ns
  where
    maxS' = max s' maxS
    maxL' = max l' maxL
    strL'
      | l' > maxL                = s'
      | l' == maxL && s' > strL  = s'
      | otherwise                = strL
    ns    = nextCmpnts fr mrkd' cs
    s'    = s + strength c
    l'    = l + 1
    mrkd' = S.insert c mrkd
    searchNext :: Cmpnt -> (Int, Int, Int) -> (Int, Int, Int)
    searchNext c' = search cs (otherEnd fr c') s' l' mrkd' c'

strength :: Cmpnt -> Int
strength (C x y _) = x + y

solution :: String -> (Int, Int)
solution input = let Just cs         = parseMaybe components input
                     (maxS, _, lenS) = search cs 0 0 (-1) S.empty (C 0 0 0) (0, 1, 0)
                  in (maxS, lenS)

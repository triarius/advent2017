{-# LANGUAGE MultiWayIf #-}

module Advent.Prob19 (solution) where

import           Advent.Lib.ParseIO
import           Control.Applicative
import           Data.AdditiveGroup
import           Data.Char (isSpace, isUpper)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import           Text.Megaparsec
import           Text.Megaparsec.Char


grid :: Parser (V.Vector (VU.Vector Char))
grid = l2Vect <$> many (manyTill anyChar eol)

-- coverts a list of list to a vector of vectors
l2Vect :: VU.Unbox a => [[a]] -> V.Vector (VU.Vector a)
l2Vect = V.fromList . map VU.fromList

unsafeLookupG :: VU.Unbox a => V.Vector (VU.Vector a) -> Int -> Int -> a
unsafeLookupG g i j = (g V.! i) VU.! j

-- find the first colunm on the first row that contains a non space char
-- this is that start of the path
start :: V.Vector (VU.Vector Char) -> (Int, Int)
start g = (0, x)
  where Just x = VU.findIndex (not . isSpace) (g V.! 0)

-- return the letters seen (in revrse order) and the number of steps in a
-- traversal of the path
followPath :: V.Vector (VU.Vector Char) -> (String, Int)
followPath g = step y0 y1 ("", 0)
  where
    y0 = y1 ^-^ (1, 0) -- the previous tile of the initial position
    y1 = start g       -- the initial position
    step :: (Int, Int) -> (Int, Int) -> (String, Int) -> (String, Int)
    step x0 x1@(i1, j1) (cs, n) = let v = x1 ^-^ x0 in
        case unsafeLookupG g i1 j1 of
          '|' -> step x1 (x1 ^+^ v) (cs, n+1)                     -- does not affect direction
          '-' -> step x1 (x1 ^+^ v) (cs, n+1)                     -- does not affect direction
          '+' -> step x1 (next x0 x1) (cs, n+1)                   -- change to neighbour tile
          ' ' -> (cs, n)                                          -- then end of the path
          c   -> if | isUpper c -> step x1 (x1 ^+^ v) (c:cs, n+1) -- does not affect direction
                    | otherwise -> error "invalid char"

    -- then next tile from a '+' is the neighbour tile that we did not come from
    -- the first argument is the previous tile and the 2nd is the current tile
    next :: (Int, Int) -> (Int, Int) -> (Int, Int)
    next x = head . filter (/= x) . neighbours

    -- returns all valid neighbours of a give tile that are not space chars
    neighbours :: (Int, Int) -> [(Int, Int)]
    neighbours (i, j) = filter ((&&) <$> inBounds <*> (not . isSpace . uncurry (unsafeLookupG g)))
                        [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
    -- test whether a tile is within the bounds of the grid
    inBounds :: (Int, Int) -> Bool
    inBounds (i, j) = 0 <= i && i < iMax && 0 <= j && j < jMax
    iMax = V.length g             -- the upper bound on rows
    jMax = VU.length (g V.! 0)    -- the upper bound on  colunms


solution :: String -> (String, Int)
solution input = let Just g = parseMaybe grid input
                     (str, n) = followPath g
                  in (reverse str, n)

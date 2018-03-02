module Advent.Prob05 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((&&&))
import           Control.Monad.ST (runST, ST)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

lenOffsets :: Parser (Vector Int)
lenOffsets = V.fromList <$> offsets
  where
    offsets = signedInt `endBy` eol
    signedInt = L.signed sc (lexeme L.decimal)
    lexeme    = L.lexeme sc
    sc        = skipMany (char ' ')

run :: Vector Int -> Int
run os = runST $ do
    vs <- V.thaw os
    (+(-1)) . fst <$> jump (1,0) vs

jump :: (Int, Int) -> STVector s Int -> ST s (Int, Int)
jump (n, pos) os
  | pos >= MV.length os = return (n, pos)
  | otherwise           = do
      o <- MV.read os pos
      MV.write os pos (o+1)
      jump (n+1, pos+o) os

run2 :: Vector Int -> Int
run2 os = runST $ do
    vs <- V.thaw os
    (+(-1)) . fst <$> jump2 (1,0) vs

jump2 :: (Int, Int) -> STVector s Int -> ST s (Int, Int)
jump2 (n, pos) os
  | pos >= MV.length os = return (n, pos)
  | otherwise           = do
      o <- MV.read os pos
      MV.write os pos (if o >= 3 then o - 1 else o + 1)
      jump2 (n+1, pos+o) os

solution :: String -> (Int, Int)
solution input = let Just vs = parseMaybe lenOffsets input
                  in (run &&& run2) vs

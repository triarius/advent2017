module Advent.Prob06 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((&&&))
import           Control.Monad.ST (runST, ST)
import           Control.Monad
import           Data.STRef
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

banks :: Parser (Vector Int)
banks = V.fromList <$> L.decimal `sepBy` (char ' ' <|> char '\t')

redist :: STVector s Int -> ST s ()
redist v = do
    let l = M.length v
    m <- maxIndexST v
    n <- M.read v m
    M.write v m 0
    mapM_ (\i -> M.modify v (+1) ((m + i) `rem` l)) [1..n]

maxIndexST :: STVector s Int -> ST s Int
maxIndexST v = do
    m <- newSTRef 0
    forM_ [0..M.length v - 1] $ \i -> do
        vm <- M.read v =<< readSTRef m
        vi <- M.read v i
        when (vi > vm) (writeSTRef m i)
    readSTRef m

cycleRedist :: Int -> Set (Vector Int) -> STVector s Int -> ST s Int
cycleRedist n seen v = do
    redist v
    v' <- V.freeze v
    if S.member v' seen then return n else cycleRedist (n+1) (S.insert v' seen) v

part1 :: Vector Int -> Int
part1 v = runST $ cycleRedist 1 (S.singleton v) =<< V.thaw v

part2 :: Vector Int -> Int
part2 v = runST $ do
    v' <- V.thaw v
    _ <- cycleRedist 1 (S.singleton v) v'
    v'' <- V.freeze v'
    cycleRedist 1 (S.singleton v'') v'

solution :: String -> (Int, Int)
solution input = let Just v = parseMaybe banks input
                  in (part1 &&& part2) v

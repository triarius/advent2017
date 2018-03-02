{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Prob12 (solution) where

import           Advent.Lib.ParseIO
import           Advent.Lib.UnionFind       (UnionFind(..), MUnionFind(..))
import qualified Advent.Lib.UnionFind       as U
import           Control.Arrow              ((&&&))
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import qualified Data.IntSet                as S
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


adjListP :: Parser [(Int, [Int])]
adjListP = adj `endBy` eol
  where
    adj = (,) <$> posInt <* symbol "<->" <*> posInt `sepBy` comma
    sc = skipMany (char ' ')
    lexeme = L.lexeme sc
    posInt = lexeme L.decimal
    symbol = L.symbol sc
    comma = symbol ","

performUnions :: [(Int, [Int])] -> UnionFind Int
performUnions adjList = runST $ do
    let n = length adjList
    (uf :: MUnionFind (STArray s) Int (ST s)) <- U.new (0, n-1)
    mapM_ (\(v, vadjs) -> mapM_ (U.union uf v) vadjs) adjList
    U.flatten uf
    pure =<< U.freeze uf

cc :: Int -> UnionFind Int -> Int
cc n = (! n) . sizes

pi0 :: UnionFind Int -> Int
pi0 = S.size . S.fromList . elems . parents

solution :: String -> (Int, Int)
solution input = let Just adjList = parseMaybe adjListP input
                  in cc 0 &&& pi0 $ performUnions adjList

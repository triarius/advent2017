module Advent.Prob09 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((&&&))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Group = Group [Group] | Garbage String deriving Show

group :: Parser Group
group = garbage <|> Group <$> curls ((garbage <|> group) `sepBy` symbol ",")
  where
    garbage :: Parser Group
    garbage = Garbage <$> (symbol "<" >> manyTill (lexeme anyChar) (symbol ">"))
    symbol :: String -> Parser String
    symbol = L.symbol sc
    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc
    sc :: Parser ()
    sc = skipMany (char '!' >> anyChar)
    curls :: Parser a -> Parser a
    curls = between (symbol "{") (symbol "}")

score :: Group -> Int
score = score' 1
  where
    score' :: Int -> Group -> Int
    score' _   (Garbage _) = 0
    score' acc (Group gs)  = acc + (sum . map (score' (acc+1)) $ gs)

countGarbage :: Group -> Int
countGarbage (Garbage xs) = length xs
countGarbage (Group gs)   = sum . map countGarbage $ gs

solution :: String -> (Int, Int)
solution input = let Just grp = parseMaybe group input
                  in (score &&& countGarbage) grp

module Advent.Prob10 (solution) where

import           Advent.Lib.ParseIO
import           Advent.Lib.KnotHash (hashRound, knotHash)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

lengths :: Parser [Int]
lengths = between (pure ()) eol (integer `sepBy` char ',')
  where integer = lexeme L.decimal
        lexeme = L.lexeme (pure ())

line :: Parser String
line = lexeme (many (notChar '\n'))
  where lexeme = L.lexeme (skipMany eol)

testHash :: Int -> [Int] -> Int
testHash len input = x * y
  where [x,y] = take 2 $ drop start xs
        (start, _, xs) = hashRound len input (0, 0, [0..len-1])

solution :: String -> (Int, String)
solution input = let Just inputLengths = parseMaybe lengths input
                     Just inputAscii   = parseMaybe line input
                  in (testHash 256 inputLengths, knotHash 256 inputAscii)

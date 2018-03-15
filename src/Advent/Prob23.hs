module Advent.Prob23 (solution) where

import           Advent.Lib.ParseIO
import           Advent.Lib.Primes (isPrime)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type IntegerOrReg = Either Int Char

data Instruction = Set Char IntegerOrReg
                 | Sub Char IntegerOrReg
                 | Mul Char IntegerOrReg
                 | Jnz IntegerOrReg IntegerOrReg

-- return a value or lookup the value in a registor
(!|) :: M.Map Char Int -> IntegerOrReg -> Int
(!|) rs = either id (rs !%)

-- lookup a value in a register, defaulting to 0
(!%) :: M.Map Char Int -> Char -> Int
(!%) = flip (M.findWithDefault 0)

instructions :: Parser (Vector Instruction)
instructions = V.fromList <$> endBy cell eol
  where
    cell = choice [ string "set " >> Set <$> letterChar <*> intOrRegP
                  , string "sub " >> Sub <$> letterChar <*> intOrRegP
                  , string "mul " >> Mul <$> letterChar <*> intOrRegP
                  , string "jnz " >> Jnz <$> intOrRegP  <*> intOrRegP ]
    intOrRegP = space >> eitherP signedInt letterChar
    signedInt = L.signed (pure ()) L.decimal

runInstructions :: Vector Instruction -> (Map Char Int, Int)
runInstructions instrs = exec 0 (M.empty, 0)
  where
    exec :: Int -> (Map Char Int, Int) -> (Map Char Int, Int)
    exec i (regs, n)
      | i < 0 || i >= V.length instrs = (regs, n)
      | otherwise                     = case instrs V.! i of
            Set r val -> exec (i+1) (M.insert r (regs !| val) regs, n)
            Sub r val -> exec (i+1) (M.adjust (+(-(regs !| val))) r regs, n)
            Mul r val -> exec (i+1) (M.adjust (*(regs !| val)) r regs, n + 1)
            Jnz val off -> if regs !| val == 0 then exec (i+1) (regs, n)
                                               else exec (i + regs !| off) (regs, n)

p2 :: Int
p2 = length . filter (not . isPrime) $ [106500,106500+17..123500]

solution :: String -> (Int, Int)
solution input = let Just instrs = parseMaybe instructions input
                     (_, p1) = runInstructions instrs
                  in (p1, p2)

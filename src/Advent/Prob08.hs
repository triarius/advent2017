module Advent.Prob08 (solution) where

import           Advent.Lib.ParseIO
import           Data.Maybe                 (fromMaybe)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Reg   = String
data Instr = Instr Reg Dir Int Cond deriving Show
data Dir   = Inc | Dec deriving Show
data Cond  = Cond Reg Compr Int deriving Show
data Compr = GRT | LST | EQU | GEQ | LEQ | NEQ deriving Show

instructions :: Parser [Instr]
instructions = instruction `endBy` eol
  where
    instruction = Instr <$> many letterChar <* space <*> dir <* space <*> signedInt <*> cond
    dir = choice [ string "inc" >> pure Inc
                 , string "dec" >> pure Dec ]
    cond = Cond <$> (string "if" >> space >> many letterChar)
                <*> (space >> compr)
                <*> (space >> signedInt)
    compr = choice [ string "> "  >> pure GRT
                   , string "< "  >> pure LST
                   , string "== " >> pure EQU
                   , string ">= " >> pure GEQ
                   , string "<= " >> pure LEQ
                   , string "!= " >> pure NEQ ]
    signedInt = L.signed sc (lexeme L.decimal)
    lexeme = L.lexeme sc
    sc = skipMany (char ' ')

process :: (Int, Map Reg Int) -> Instr -> (Int, Map Reg Int)
process (maxVal, regs) (Instr r dir val (Cond cr comp cx))
  = if evalCond then (maxVal', regs') else (maxVal, regs)
  where
    regs' = M.alter (pure . (+adjVal) . fromMaybe 0) r regs
    maxVal' = max (M.findWithDefault 0 r regs') maxVal
    evalCond = let cv = M.findWithDefault 0 cr regs
                in case comp of
                     GRT -> cv >  cx
                     LST -> cv <  cx
                     EQU -> cv == cx
                     GEQ -> cv >= cx
                     LEQ -> cv <= cx
                     NEQ -> cv /= cx
    adjVal = case dir of
               Inc -> val
               Dec -> -val

run :: [Instr] -> (Int, Map Reg Int)
run = foldl process (minBound, M.empty)

largest :: Map Reg Int -> Int
largest = M.foldr max minBound

solution :: String -> (Int, Int)
solution input = let Just is = parseMaybe instructions input
                     (histMax, result) = run is
                  in (largest result, histMax)

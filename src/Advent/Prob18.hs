module Advent.Prob18 (solution) where

import           Advent.Lib.ParseIO
import qualified Advent.Lib.Queue           as Q
import           Control.Arrow              ((&&&))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (signed, decimal)

type IntegerOrReg = Either Int Char

data Instruction = Snd IntegerOrReg
                 | Set Char IntegerOrReg
                 | Add Char IntegerOrReg
                 | Mul Char IntegerOrReg
                 | Mod Char IntegerOrReg
                 | Rcv IntegerOrReg
                 | Jgz IntegerOrReg IntegerOrReg
    deriving Show

data Proc = Proc Int (M.Map Char Int) (Q.Queue Int) Bool
    deriving Show

genProc :: Int -> Proc
genProc pid = Proc 0 (M.singleton 'p' pid) Q.empty False

-- return a value or lookup the value in a register
(!|) :: M.Map Char Int -> IntegerOrReg -> Int
(!|) rs = either id (rs !%)

-- lookup a value in a register, defaulting to 0
(!%) :: M.Map Char Int -> Char -> Int
(!%) = flip (M.findWithDefault 0)

-- read a file and parse it for instructions
instructions :: Parser (Vector Instruction)
instructions = V.fromList <$> endBy cell eol
  where
    cell = choice [ string "snd " >> Snd <$> intOrRegP
                  , string "set " >> Set <$> letterChar <*> intOrRegP
                  , string "add " >> Add <$> letterChar <*> intOrRegP
                  , string "mul " >> Mul <$> letterChar <*> intOrRegP
                  , string "mod " >> Mod <$> letterChar <*> intOrRegP
                  , string "rcv " >> Rcv <$> intOrRegP
                  , string "jgz " >> Jgz <$> intOrRegP <*> intOrRegP ]
    intOrRegP = space >> eitherP signedInt letterChar
    signedInt = signed (return ()) decimal

-- part 1
runInstructions :: Vector Instruction -> Maybe Int
runInstructions instrs = exec 0 M.empty Nothing
  where
    exec :: Int -> M.Map Char Int -> Maybe Int -> Maybe Int
    exec i rs prev
      = case instrs V.! i of
          Snd val     -> exec (i + 1) rs (Just $ rs !| val)
          Set r val   -> exec (i + 1) (M.insert r (rs !| val) rs) prev
          Add r val   -> exec (i + 1) (M.adjust (+ rs !| val) r rs) prev
          Mul r val   -> exec (i + 1) (M.adjust (* rs !| val) r rs) prev
          Mod r val   -> exec (i + 1) (M.adjust (`rem` rs !| val) r rs) prev
          Rcv val     -> if rs !| val /= 0 then prev else exec (i + 1) rs prev
          Jgz val off -> if rs !| val >  0 then exec (i + rs !| off) rs prev
                                           else exec (i + 1) rs prev
-- part2
runInstructions2 :: Vector Instruction -> Int
runInstructions2 instrs = exec 0 (genProc 0) (genProc 1) 0
  where
    exec :: Int -> Proc -> Proc -> Int -> Int
    exec pid p0@(Proc i0 rs0 q0 s0) p1@(Proc i1 rs1 q1 s1) n
      = let instr = instrs V.! i0
         in case instr of
              Snd val -> exec (inc2 pid) (Proc i1 rs1 (Q.snoc q1 (rs0 !| val)) s1)
                            (Proc (i0+1) rs0 q0 s0) (n+pid)
              Set _ _ -> exec (inc2 pid) p1 (modifyReg p0 instr) n
              Add _ _ -> exec (inc2 pid) p1 (modifyReg p0 instr) n
              Mul _ _ -> exec (inc2 pid) p1 (modifyReg p0 instr) n
              Mod _ _ -> exec (inc2 pid) p1 (modifyReg p0 instr) n
              Rcv (Right r)
                | s0 && s1  -> n
                | Q.null q0 -> exec (inc2 pid) p1 (Proc i0 rs0 q0 True) n
                | otherwise -> exec (inc2 pid) p1
                                (Proc (i0+1) (M.insert r (Q.head q0) rs0) (Q.tail q0) False) n
              Rcv (Left _)  -> error "invalid register for receipt"
              Jgz val off
                | rs0 !| val > 0 -> exec (inc2 pid) p1 (Proc (i0+rs0 !| off) rs0 q0 s0) n
                | otherwise      -> exec (inc2 pid) p1 (Proc (i0+1) rs0 q0 s0) n

-- apply the instructions that modify a register exclusively
modifyReg :: Proc -> Instruction -> Proc
modifyReg (Proc i rs q s) (Set r val) = Proc (i+1) (M.insert r (rs !| val) rs) q s
modifyReg (Proc i rs q s) (Add r val) = Proc (i+1) (M.adjust (+ rs !| val) r rs) q s
modifyReg (Proc i rs q s) (Mul r val) = Proc (i+1) (M.adjust (* rs !| val) r rs) q s
modifyReg (Proc i rs q s) (Mod r val) = Proc (i+1) (M.adjust (`rem` rs !| val) r rs) q s
modifyReg _ _                         = error "invalid instruction"

-- +1 mod 2
inc2 :: Int -> Int
inc2 = (`rem` 2) . (+1)

solution :: String -> (Int, Int)
solution input = let Just instrs = parseMaybe instructions input
                  in (fromJust . runInstructions &&& runInstructions2) instrs

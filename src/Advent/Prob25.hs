module Advent.Prob25 (solution) where

import           Advent.Lib.ParseIO
import           Data.List (foldl')
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Map (Map)
import qualified Data.Map as M
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type State         = Char
data Dir           = L | R deriving (Eq, Ord, Show)
type Transitions   = Map (State, Int) (State, Int, Dir)
data TuringMachine = TM { start :: State
                        , diag :: Int
                        , transitions :: Transitions
                        } deriving (Show)

turingMachineP :: Parser TuringMachine
turingMachineP = TM <$> extractNth 2 upperChar <*> extract L.decimal <*> transitionsP
  where
    transitionsP :: Parser Transitions
    transitionsP = M.fromList . concatMap mkTrans <$> many transitionP
    transitionP :: Parser (State, [(Int, Int, Dir, State)])
    transitionP = (,) <$> (eol *> extractNth 2 upperChar) <*> many stateTransitionP
    stateTransitionP :: Parser (Int, Int, Dir, State)
    stateTransitionP = (,,,) <$> extract L.decimal
                             <*> extract L.decimal
                             <*> extract (choice [ string "left" >> return L
                                                 , string "right" >> return R ])
                             <*> extractNth 2 upperChar
    extract :: Parser a -> Parser a
    extract = extractNth 1
    extractNth :: Int -> Parser a -> Parser a
    extractNth n p = count (n-1) (skipManyTill (notChar '\n') p)
                        *> skipManyTill (notChar '\n') p
                        <* manyTill (notChar '\n') eol

mkTrans :: (State, [(Int, Int, Dir, State)]) -> [((State, Int), (State, Int, Dir))]
mkTrans (s, ts) = map (\(i, j, d, f) -> ((s, i), (f, j, d))) ts

tMStep :: Transitions -> (Int, IntMap Int, State) -> Int -> (Int, IntMap Int, State)
tMStep ts (pos, tape, state) _ = (pos' dir pos, tape', state')
  where
    val = IM.findWithDefault 0 pos tape
    Just (state', val', dir) = M.lookup (state, val) ts
    pos' L = (+(-1))
    pos' R = (+1)
    tape' = IM.insert pos val' tape

runTM :: TuringMachine -> Int
runTM tm = IM.foldr (+) 0 tape
  where (_, tape, _) = foldl' (tMStep $ transitions tm) (0, IM.empty, start tm) [1..diag tm]

solution :: String -> Int
solution input = let Just tm = parseMaybe turingMachineP input
                  in runTM tm

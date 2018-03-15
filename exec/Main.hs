module Main (main) where

import qualified Advent.Prob01 as Prob01
import qualified Advent.Prob02 as Prob02
import qualified Advent.Prob03 as Prob03
import qualified Advent.Prob04 as Prob04
import qualified Advent.Prob05 as Prob05
import qualified Advent.Prob06 as Prob06
import qualified Advent.Prob07 as Prob07
import qualified Advent.Prob08 as Prob08
import qualified Advent.Prob09 as Prob09
import qualified Advent.Prob10 as Prob10
import qualified Advent.Prob11 as Prob11
import qualified Advent.Prob12 as Prob12
import qualified Advent.Prob13 as Prob13
import qualified Advent.Prob14 as Prob14
import qualified Advent.Prob15 as Prob15
import qualified Advent.Prob16 as Prob16
import qualified Advent.Prob17 as Prob17
import qualified Advent.Prob18 as Prob18
import qualified Advent.Prob19 as Prob19
import qualified Advent.Prob20 as Prob20
import qualified Advent.Prob21 as Prob21
import qualified Advent.Prob22 as Prob22
import qualified Advent.Prob23 as Prob23
import qualified Advent.Prob24 as Prob24
import qualified Advent.Prob25 as Prob25
import           System.Environment
import           Text.Printf


printSol :: Int -> String -> IO ()
printSol  1 path = print =<< Prob01.solution <$> readFile path
printSol  2 path = print =<< Prob02.solution <$> readFile path
printSol  3 path = print =<< Prob03.solution <$> readFile path
printSol  4 path = print =<< Prob04.solution <$> readFile path
printSol  5 path = print =<< Prob05.solution <$> readFile path
printSol  6 path = print =<< Prob06.solution <$> readFile path
printSol  7 path = print =<< Prob07.solution <$> readFile path
printSol  8 path = print =<< Prob08.solution <$> readFile path
printSol  9 path = print =<< Prob09.solution <$> readFile path
printSol 10 path = print =<< Prob10.solution <$> readFile path
printSol 11 path = print =<< Prob11.solution <$> readFile path
printSol 12 path = print =<< Prob12.solution <$> readFile path
printSol 13 path = print =<< Prob13.solution <$> readFile path
printSol 14 path = print =<< Prob14.solution <$> readFile path
printSol 15 path = print =<< Prob15.solution <$> readFile path
printSol 16 path = print =<< Prob16.solution <$> readFile path
printSol 17 path = print =<< Prob17.solution <$> readFile path
printSol 18 path = print =<< Prob18.solution <$> readFile path
printSol 19 path = print =<< Prob19.solution <$> readFile path
printSol 20 path = print =<< Prob20.solution <$> readFile path
printSol 21 path = print =<< Prob21.solution <$> readFile path
printSol 22 path = print =<< Prob22.solution <$> readFile path
printSol 23 path = print =<< Prob23.solution <$> readFile path
printSol 24 path = print =<< Prob24.solution <$> readFile path
printSol 25 path = print =<< Prob25.solution <$> readFile path
printSol _  _    = putStrLn "Incorrect problem number."

main :: IO ()
main = do
    args <- getArgs
    case args of
      []          -> putStrLn "Usage: "
      [day]       -> printSol (read day) (printf "input/%02d/input" (read day :: Int))
      [day, path] -> printSol (read day) path
      _           -> putStrLn "Usage: "

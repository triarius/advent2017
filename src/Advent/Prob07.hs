module Advent.Prob07 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((&&&))
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type ProgTree = Map String (Int, [String])

progList :: Parser ProgTree
progList = M.fromList <$> progEntry `endBy` eol
  where progEntry = (\x y z -> (x,(y,z))) <$> (many letterChar <* space)
                                          <*> between (char '(') (char ')') L.decimal
                                          <*  many (oneOf " ->")
                                          <*> some letterChar `sepBy` string ", "

findBottom :: ProgTree -> String
findBottom pT = getRoot pT (M.foldrWithKey f M.empty pT)
  where f ::  String -> (Int, [String]) -> Map String String -> Map String String
        f name (_, cs) parents = foldr (\c ps -> M.insert c name ps) parents cs

getRoot :: ProgTree -> Map String String -> String
getRoot pT parents = snd . head . filter ((== Nothing) . fst) $ qs
  where qs = map ((`M.lookup` parents) &&& id) (M.keys pT)

mkSupps :: ProgTree -> String -> Map String Int
mkSupps pT base = mkSupps' base M.empty
  where
    mkSupps' :: String -> Map String Int -> Map String Int
    mkSupps' x ss = case ps of
                         [] -> M.insert x w ss
                         _  -> M.insert x w' remain
      where
        Just (w, ps) = M.lookup x pT
        remain = foldr mkSupps' ss ps
        w' = w + sum (map (flip (M.findWithDefault 0) remain) ps)

lastUnbalanced :: ProgTree -> Map String Int -> String -> String -> Int
lastUnbalanced pT suppMap l x
  | M.null . unbalanced . freqs . supps suppMap $ xs = wt + balWt - unbalWt
  | otherwise                                        = lastUnbalanced pT suppMap x x'
  where
    ((_, x':_):_)    = M.toList . unbalanced . freqs . supps suppMap $ xs
    ((unbalWt, _):_) = M.toList . unbalanced . freqs . supps suppMap $ ys
    ((balWt, _):_)   = M.toList . balanced . freqs . supps suppMap $ ys
    Just (wt, xs)    = M.lookup x pT
    Just (_, ys)     = M.lookup l pT

balanced :: Map Int [String] -> Map Int [String]
balanced = M.filter ((/=1) . length)

unbalanced :: Map Int [String] -> Map Int [String]
unbalanced  = M.filter ((==1) . length)

freqs :: [(String, Int)] -> Map Int [String]
freqs = foldr (uncurry f) M.empty
  where
    f :: String -> Int -> Map Int [String] -> Map Int [String]
    f prg = M.alter (pure . (prg:) . fromMaybe [])

supps :: Map String Int -> [String] -> [(String, Int)]
supps suppMap = map (id &&& flip (M.findWithDefault 0) suppMap)

solution :: String -> (String, Int)
solution input = let Just pT = parseMaybe progList input
                     b       = findBottom pT
                     ss      = mkSupps pT b
                  in (id &&& lastUnbalanced pT ss "") b

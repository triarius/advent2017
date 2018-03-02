{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Advent.Prob04 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((***))
import           Control.Monad (join)
import           Control.Monad.Loops
import           Control.Monad.ST
import qualified Data.HashTable.Class as H
import           Data.HashTable.ST.Cuckoo (HashTable)
import           Text.Megaparsec
import           Text.Megaparsec.Char

passphrases :: Parser [[String]]
passphrases = passphrase `endBy` eol
  where passphrase = many lowerChar `sepBy` char ' '

hasDups :: [String] -> Bool
hasDups strs = runST $ do
    ht :: HashTable s String Bool <- H.new
    allM (flip (H.mutate ht) f) strs
      where f Nothing  = (Just True, True)
            f (Just x) = (Just True, not x)

isAnagram :: (String, String) -> Bool
isAnagram (str1, str2) = runST $ do
    ht :: HashTable s Char Int <- H.new
    mapM_ (flip (H.mutate ht) (f (+1))) str1
    mapM_ (flip (H.mutate ht) (f (+(-1)))) str2
    H.foldM (\acc (_, v) -> return $ acc && (v==0)) True ht
      where f g Nothing  = (Just $ g 0, ())
            f g (Just x) = (Just $ g x, ())

comb2 :: [a] -> [(a,a)]
comb2 [] = []
comb2 (x:xs) = map (x,) xs ++ comb2 xs

solution :: String -> (Int, Int)
solution input = let Just ps = parseMaybe passphrases input
                     valid   = filter hasDups ps
                     valid2  = filter (not . any isAnagram . comb2) valid
                  in join (***) length (valid, valid2)

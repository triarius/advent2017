{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}

module Advent.Prob20 (solution) where

import           Advent.Lib.ParseIO
import           Control.Arrow ((&&&))
import           Data.AdditiveGroup
import           Data.Function (on)
import           Data.List (sortOn, groupBy, minimumBy)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- a Particle is determined by its (initial) position, velocity and acceleration
data Particle = Particle { pos :: (Double, Double, Double)
                         , vel :: (Double, Double, Double)
                         , acc :: (Double, Double, Double)
                         } deriving (Show, Eq, Ord)

-- we make the particles an additive group so we can calculate the distance
-- between the using the relative equation of motion
instance AdditiveGroup Particle where
  zeroV     = Particle $$$ replicate 3 zeroV
  p1 ^+^ p2 = Particle { pos = pos p1 ^+^ pos p2
                       , vel = vel p1 ^+^ vel p2
                       , acc = acc p1 ^+^ acc p2 }
  negateV p = Particle $$$ map negateV ([pos, vel, acc] <*> [p])

-- apply function (ternary) to first 3 arguements in a list
($$$) :: (a -> a -> a -> b) -> [a] -> b
f $$$ (x:y:z:_) = f x y z
_ $$$ _         = error "list too short"

-- the manhattan norm of a vector in R^3
norm :: Num a => (a, a, a) -> a
norm (x, y, z) = abs x + abs y + abs z

particles :: Parser [Particle]
particles = endBy particle eol
  where
    particle  = Particle <$ posP <*> tuple <* comma <* velP <*> tuple <* comma <* accP <*> tuple
    tuple     = angles $ (,,) <$> signedDbl <* comma <*> signedDbl <* comma <*> signedDbl
    angles    = between (symbol "<") (symbol ">")
    comma     = symbol ","
    posP      = symbol "p="
    velP      = symbol "v="
    accP      = symbol "a="
    symbol    = L.symbol sc
    signedDbl = fromInteger <$> L.signed sc (lexeme L.decimal)
    lexeme    = L.lexeme sc
    sc        = skipMany (char ' ')

step :: Particle -> Particle
step p = p { pos = x', vel = v' }
  where v' = vel p ^+^ acc p
        x' = pos p ^+^ v'

resolveColllisions :: [Particle] -> [Particle]
resolveColllisions = concat . filter ((==1) . length) . groupBy ((==) `on` pos) . sortOn pos

runSim :: [Particle] -> [[Particle]]
runSim = iterate (resolveColllisions . map step)

part1 :: [Particle] -> Int
part1 = fst . minimumBy (compare `on` norm . pos . snd) . zip [0..] . (!! 1000) . iterate (map step)

part2 :: [Particle] -> Int
part2 = length . (!! 1000) . runSim

solution :: String -> (Int, Int)
solution input = let Just ps = parseMaybe particles input
                  in (part1 &&& part2) ps

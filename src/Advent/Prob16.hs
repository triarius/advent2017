{-# LANGUAGE DataKinds, ScopedTypeVariables, TupleSections#-}

module Advent.Prob16 (solution) where

import           Advent.Lib.Misc (pairMap)
import           Advent.Lib.ParseIO
import           Advent.Lib.Symmetric
import           Control.Arrow ((&&&))
import           GHC.TypeNats
import           Data.Semigroup
import qualified Data.Vector.Unboxed as V
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- The group Move n is the direct product of the opposite group of
-- Sym n and Sym n. This reflects that fact that the spin and
-- exchange moves act on the type Finte n -> A, where A is a the finite subtype
-- of Char that consists of 'a', ..., 'p', by right composition while the
-- parter move acts by left composition, the former is a right action, which we
-- will treat as a left action by the opposite group
data Move n = Move (Sym n) (Sym n)
    deriving (Eq, Ord, Read, Show)
instance KnownNat n => Semigroup (Move n) where
    Move g1 g2 <> Move h1 h2 = Move (h1 <> g1) (g2 <> h2)
instance KnownNat n => Monoid (Move n) where
    mempty  = Move mempty mempty
    mappend = (<>)

-- the spin move is simply the map \x -> x - k `mod` l. The `mod` is
-- automatically applied by mkSym
spinMove :: KnownNat n => Int -> Move n
spinMove k = Move (mkSym $ int2Fin . (+(-k)) . fin2Int) mempty

-- the exchange move is a transposition in the first factor
exchMove :: KnownNat n => Int -> Int -> Move n
exchMove i j = Move (swap (int2Fin i) (int2Fin j)) mempty

-- the parterMove is in the second factor
partnerMove :: forall n . KnownNat n => Char -> Char -> Move n
partnerMove x y = Move mempty (swap (idx x) (idx y))
  where idx z = int2Fin (fromEnum z - fromEnum 'a')

-- parse a string into a list of Move n
movesP :: KnownNat n => Parser [Move n]
movesP = between (pure ()) eol line
  where line = sepBy cell (char ',')
        cell :: KnownNat n => Parser (Move n)
        cell = choice
            [ char 's' >> spinMove <$> L.decimal
            , char 'x' >> exchMove <$> L.decimal <*> (char '/' >> L.decimal)
            , char 'p' >> partnerMove <$> letterChar <*> (char '/' >> letterChar) ]

-- the order of an element in the Move n group
orderMove :: KnownNat n => Move n -> Int
orderMove (Move g1 g2) = lcm (order g1) (order g2)

-- the action of Move n on the set of program configurations. Mathematically,
-- a configuration should be a fucntion from Finite n -> A (see above for A)
-- For convienence, we shall settle for a Vector of Chars
moveAction :: KnownNat n => Move n -> V.Vector Char -> V.Vector Char
moveAction (Move (Sym v1) g2) = V.map (perm2CharEndo g2) . flip V.backpermute v1

-- converts a permuation to a endomorphism of Char, with 0 corresponding to a
perm2CharEndo :: KnownNat n => Sym n -> Char -> Char
perm2CharEndo (Sym v) = toEnum . (+aVal) . (v V.!) . (+(-aVal)) . fromEnum
  where aVal = fromEnum 'a'

solution :: String -> (String, String)
solution input = let Just moves = parseMaybe movesP input :: Maybe [Move 16] -- read the moves
                     move       = mconcat moves                   -- multuply all the moves together
                     o          = orderMove move                  -- the order of the aggreate move
                     v          = V.fromList ['a'..'p']           -- the starting point
                     m          = 10^(9 :: Int) `rem` o
                  in pairMap V.toList
                   . pairMap (uncurry moveAction)
                   . pairMap (, v)
                   $ (move, stimes m move)

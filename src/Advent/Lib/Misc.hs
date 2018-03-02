module Advent.Lib.Misc where

import Control.Arrow ((***))
import Control.Monad (join)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap = join (***)

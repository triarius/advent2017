{-# LANGUAGE FlexibleInstances #-}

module Advent.Lib.ParseIO (Parser) where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String

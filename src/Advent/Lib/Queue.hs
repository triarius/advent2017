module Advent.Lib.Queue
    ( Queue(..)
    , empty
    , Advent.Lib.Queue.null
    , snoc
    , Advent.Lib.Queue.head
    , Advent.Lib.Queue.tail
    ) where

data Queue a = Queue [a] [a]
    deriving Show

empty :: Queue a
empty = Queue [] []

null :: Queue a -> Bool
null (Queue [] _) = True
null _            = False

check :: Queue a -> Queue a
check (Queue [] ys) = Queue (reverse ys) []
check q             = q

snoc :: Queue a -> a -> Queue a
snoc (Queue xs ys) y = check $ Queue xs (y:ys)

head :: Queue a -> a
head (Queue [] _) = error "empty queue"
head (Queue (x:_) _) = x

tail :: Queue a -> Queue a
tail (Queue [] _) = error "empty queue"
tail (Queue (_:xs) ys) = check $ Queue xs ys

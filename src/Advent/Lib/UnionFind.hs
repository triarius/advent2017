{-# LANGUAGE Rank2Types, FlexibleContexts, ScopedTypeVariables #-}

module Advent.Lib.UnionFind ( UnionFind(..)
                            , MUnionFind(..)
                            , new
                            , union
                            , find
                            , flatten
                            , freeze
                            , thaw
                            , generate
                            ) where

import           Control.Arrow ((&&&))
import           Data.Array (Array)
import           Data.Ix
import           Data.Array.MArray (MArray)
import qualified Data.Array.MArray as M

data UnionFind i = UnionFind { parents :: Array i i
                             , sizes :: Array i Int
                             } deriving Show

data MUnionFind a i m = MUnionFind { mparents :: MArray a i m => a i i
                                   , msizes :: MArray a Int m => a i Int }

generate :: (MArray a e m, Ix i) => (i, i) -> (i -> e) -> m (a i e)
generate bounds f = do
    a <- M.newArray_ bounds
    mapM_ (uncurry (M.writeArray a) . (id &&& f)) (range bounds)
    pure a

new :: (MArray a Int m, MArray a i m, Ix i) => (i, i) -> m (MUnionFind a i m)
new bounds = do
    as <- generate bounds id
    ss <- M.newArray bounds 1
    pure $ MUnionFind as ss

find :: (MArray a i m, Ix i) => MUnionFind a i m -> i -> m i
find uf v = do
    parentV <- M.readArray (mparents uf) v
    parent2V <- M.readArray (mparents uf) parentV
    M.writeArray (mparents uf) v parent2V
    if parent2V == parentV then pure parentV else find uf parent2V

union :: (MArray a Int m, MArray a i m, Ix i) => MUnionFind a i m -> i -> i -> m ()
union uf v w = do
    let as = mparents uf
        ss = msizes uf
    rootV <- find uf v
    rootW <- find uf w
    if rootV /= rootW
       then do
           szV <- M.readArray ss rootV
           szW <- M.readArray ss rootW
           if szV <= szW
              then do
                  M.writeArray as rootV rootW
                  M.writeArray (msizes uf) rootW (szV + szW)
              else do
                  M.writeArray as rootW rootV
                  M.writeArray (msizes uf) rootV (szV + szW)
        else pure ()

flatten :: (MArray a Int m, MArray a i m, Ix i) => MUnionFind a i m -> m ()
flatten uf = do
    bounds <- M.getBounds $ mparents uf
    mapM_ (\i -> do
            x <- M.readArray (mparents uf) i
            x' <- find uf x
            sz <- M.readArray (msizes uf) x'
            M.writeArray (mparents uf) i x'
            M.writeArray (msizes uf) i sz
          ) $ range bounds

freeze :: (MArray a Int m, MArray a i m, Ix i) => MUnionFind a i m -> m (UnionFind i)
freeze uf = do
    ps <- M.freeze (mparents uf)
    ss <- M.freeze (msizes uf)
    pure $ UnionFind ps ss

thaw :: (MArray a Int m, MArray a i m, Ix i) => UnionFind i -> m (MUnionFind a i m)
thaw uuf = do
    ps <- M.thaw (parents uuf)
    ss <- M.thaw (sizes uuf)
    pure $ MUnionFind ps ss

{-# LANGUAGE FlexibleContexts #-}

module UnionFind where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray (MArray, getAssocs, newArray, readArray, writeArray)
import Data.Ix (Ix)

data UnionFind arr s i = UnionFind
  { ufParent :: arr s i i,
    ufSize :: arr s i Int
  }

newUF :: (Ix i, MArray (arr s) i (ST s), MArray (arr s) Int (ST s)) => (i, i) -> ST s (UnionFind arr s i)
newUF bounds = do
  parent <- newArray bounds undefined
  size <- newArray bounds 1
  assocs <- getAssocs size
  forM_ assocs $ \(idx, _) -> writeArray parent idx idx
  return $ UnionFind parent size

getRoot :: (Ix i, Eq i, MArray (arr s) i (ST s)) => UnionFind arr s i -> i -> ST s i
getRoot uf x = do
  p <- readArray (ufParent uf) x
  if p == x
    then return x
    else do
      p' <- getRoot uf p
      writeArray (ufParent uf) x p'
      return p'

unite :: (Ix i, Eq i, MArray (arr s) i (ST s), MArray (arr s) Int (ST s)) => UnionFind arr s i -> i -> i -> ST s ()
unite uf x y = do
  x' <- getRoot uf x
  y' <- getRoot uf y
  when (x' /= y') $ do
    sizeX <- readArray (ufSize uf) x'
    sizeY <- readArray (ufSize uf) y'
    if sizeX > sizeY
      then do
        writeArray (ufParent uf) y' x'
        writeArray (ufSize uf) x' (sizeX + sizeY)
      else do
        writeArray (ufParent uf) x' y'
        writeArray (ufSize uf) y' (sizeX + sizeY)

sameUF :: (Ix i, Eq i, MArray (arr s) i (ST s)) => UnionFind arr s i -> i -> i -> ST s Bool
sameUF uf x y = (==) <$> getRoot uf x <*> getRoot uf y

getSize :: (Ix i, Eq i, MArray (arr s) i (ST s), MArray (arr s) Int (ST s)) => UnionFind arr s i -> i -> ST s Int
getSize uf x = do
  r <- getRoot uf x
  readArray (ufSize uf) r

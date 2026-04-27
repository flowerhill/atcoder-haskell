{-# LANGUAGE FlexibleContexts #-}

module UnionFind where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray (MArray, newArray, newListArray, readArray, writeArray)
import Data.Ix (Ix, range)

data UnionFind arr s i = UnionFind
  { ufParent :: arr s i i,
    ufSize :: arr s i Int
  }

-- | Union-Find を初期化する（各要素が独立した集合）
--
-- >>> import Data.Array.ST (STArray)
-- >>> runST $ do { uf <- newUF (1,3) :: ST s (UnionFind STArray s Int); getRoot uf 1 }
-- 1
newUF bounds = do
  parent <- newListArray bounds (range bounds)
  size <- newArray bounds 1
  return $ UnionFind parent size

-- | 要素の根（代表元）を経路圧縮しながら返す
--
-- >>> import Data.Array.ST (STArray)
-- >>> runST $ do { uf <- newUF (1,3) :: ST s (UnionFind STArray s Int); unite uf 1 2; getRoot uf 1 }
-- 2
getRoot :: (Ix i, Eq i, MArray (arr s) i (ST s)) => UnionFind arr s i -> i -> ST s i
getRoot uf x = do
  p <- readArray (ufParent uf) x
  if p == x
    then return x
    else do
      p' <- getRoot uf p
      writeArray (ufParent uf) x p'
      return p'

-- | 2つの集合を統合する（Union by size）
--
-- >>> import Data.Array.ST (STArray)
-- >>> runST $ do { uf <- newUF (1,4) :: ST s (UnionFind STArray s Int); unite uf 1 2; unite uf 3 4; sameUF uf 1 3 }
-- False
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

-- | 2つの要素が同じ集合に属するか判定する
--
-- >>> import Data.Array.ST (STArray)
-- >>> runST $ do { uf <- newUF (1,3) :: ST s (UnionFind STArray s Int); unite uf 1 2; sameUF uf 1 2 }
-- True
-- >>> import Data.Array.ST (STArray)
-- >>> runST $ do { uf <- newUF (1,3) :: ST s (UnionFind STArray s Int); sameUF uf 1 3 }
-- False
sameUF :: (Ix i, Eq i, MArray (arr s) i (ST s)) => UnionFind arr s i -> i -> i -> ST s Bool
sameUF uf x y = (==) <$> getRoot uf x <*> getRoot uf y

-- | 要素が属する集合のサイズを返す
--
-- >>> import Data.Array.ST (STArray)
-- >>> runST $ do { uf <- newUF (1,4) :: ST s (UnionFind STArray s Int); unite uf 1 2; unite uf 1 3; getSize uf 1 }
-- 3
getSize :: (Ix i, Eq i, MArray (arr s) i (ST s), MArray (arr s) Int (ST s)) => UnionFind arr s i -> i -> ST s Int
getSize uf x = do
  r <- getRoot uf x
  readArray (ufSize uf) r

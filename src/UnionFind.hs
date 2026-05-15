{-# LANGUAGE FlexibleContexts #-}

module UnionFind where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array.IArray (Array, listArray, (!))
import Data.Array.MArray (MArray, newArray, readArray, writeArray)
import Data.Ix (Ix, index, range, rangeSize)

-- | UnionFind: 内部 storage は flat Int、API は任意の Ix index。
--
-- STUArray を使えば parent も size も unboxed のまま、(Int,Int) 等の
-- 多次元 index で扱える。
data UnionFind arr s i = UnionFind
  { ufBounds :: !(i, i),
    ufParent :: !(arr s Int Int), -- 親の flat index を格納
    ufSize :: !(arr s Int Int),
    ufFromInt :: Array Int i -- flat Int -> i 逆引き (lazy)
  }

-- | Union-Find を初期化する（各要素が独立した集合）
--
-- >>> import Data.Array.ST (STUArray)
-- >>> runST $ do { uf <- newUF ((1,1),(2,2)) :: ST s (UnionFind STUArray s (Int,Int)); getRoot uf (1,1) }
-- (1,1)
newUF :: (Ix i, MArray (arr s) Int (ST s)) => (i, i) -> ST s (UnionFind arr s i)
newUF bs = do
  let n = rangeSize bs
      inv = listArray (0, n - 1) (range bs)
  parent <- newArray (0, n - 1) 0
  size <- newArray (0, n - 1) 1
  forM_ [0 .. n - 1] $ \k -> writeArray parent k k
  return $ UnionFind bs parent size inv

-- | 内部用: flat Int 上で根を探し、経路圧縮する
--
-- >>> import Data.Array.ST (STUArray)
-- >>> runST $ do { uf <- newUF ((1,1),(2,2)) :: ST s (UnionFind STUArray s (Int,Int)); getRootFlat uf 0 }
-- 0
getRootFlat :: (MArray (arr s) Int (ST s)) => UnionFind arr s i -> Int -> ST s Int
getRootFlat uf k = do
  p <- readArray (ufParent uf) k
  if p == k
    then return k
    else do
      r <- getRootFlat uf p
      writeArray (ufParent uf) k r
      return r

-- | 要素の根（代表元）を経路圧縮しながら返す
--
-- >>> import Data.Array.ST (STUArray)
-- >>> runST $ do { uf <- newUF ((1,1),(2,2)) :: ST s (UnionFind STUArray s (Int,Int)); unite uf (1,1) (2,2); getRoot uf (1,1) }
-- (2,2)
getRoot :: (Ix i, MArray (arr s) Int (ST s)) => UnionFind arr s i -> i -> ST s i
getRoot uf x = do
  rFlat <- getRootFlat uf (index (ufBounds uf) x)
  return $ ufFromInt uf ! rFlat

-- | 2つの集合を統合する（Union by size）
--
-- >>> import Data.Array.ST (STUArray)
-- >>> runST $ do { uf <- newUF ((1,1),(2,2)) :: ST s (UnionFind STUArray s (Int,Int)); unite uf (1,1) (1,2); unite uf (2,1) (2,2); sameUF uf (1,1) (2,1) }
-- False
unite :: (Ix i, MArray (arr s) Int (ST s)) => UnionFind arr s i -> i -> i -> ST s ()
unite uf x y = do
  let bs = ufBounds uf
  rx <- getRootFlat uf (index bs x)
  ry <- getRootFlat uf (index bs y)
  when (rx /= ry) $ do
    sx <- readArray (ufSize uf) rx
    sy <- readArray (ufSize uf) ry
    if sx > sy
      then do
        writeArray (ufParent uf) ry rx
        writeArray (ufSize uf) rx (sx + sy)
      else do
        writeArray (ufParent uf) rx ry
        writeArray (ufSize uf) ry (sx + sy)

-- | 2つの要素が同じ集合に属するか判定する
--
-- >>> import Data.Array.ST (STUArray)
-- >>> runST $ do { uf <- newUF ((1,1),(2,2)) :: ST s (UnionFind STUArray s (Int,Int)); unite uf (1,1) (1,2); sameUF uf (1,1) (1,2) }
-- True
-- >>> import Data.Array.ST (STUArray)
-- >>> runST $ do { uf <- newUF ((1,1),(2,2)) :: ST s (UnionFind STUArray s (Int,Int)); sameUF uf (1,1) (2,2) }
-- False
sameUF :: (Ix i, MArray (arr s) Int (ST s)) => UnionFind arr s i -> i -> i -> ST s Bool
sameUF uf x y = do
  let bs = ufBounds uf
  rx <- getRootFlat uf (index bs x)
  ry <- getRootFlat uf (index bs y)
  return $ rx == ry

-- | 要素が属する集合のサイズを返す
--
-- >>> import Data.Array.ST (STUArray)
-- >>> runST $ do { uf <- newUF ((1,1),(2,2)) :: ST s (UnionFind STUArray s (Int,Int)); unite uf (1,1) (1,2); unite uf (1,1) (2,1); getSize uf (1,1) }
-- 3
getSize :: (Ix i, MArray (arr s) Int (ST s)) => UnionFind arr s i -> i -> ST s Int
getSize uf x = do
  r <- getRootFlat uf (index (ufBounds uf) x)
  readArray (ufSize uf) r

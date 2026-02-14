{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module WUnionFind where

import Control.Monad (when)
import Data.Array (index)
import Data.Array.Base (MArray (..), readArray, writeArray)
import Data.Bool (bool)
import Data.IORef (IORef, modifyIORef', newIORef)
import Data.Ix (Ix)

-- modifyArray の互換実装 (array < 0.5.6.0 向け)
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

{-- 重み付き Union-Find --}
-- ref: https://qiita.com/drken/items/cce6fc5c579051e64fab

data WeightedUnionFind arr i
  = WeightedUnionFind
      (arr i i) -- 親頂点 / rep は代表元
      (arr i Int) -- 集合サイズ (代表元で検索する)
      (arr i Int) -- weight
      (IORef Int) -- 連結成分数
      i -- 代表元 (representative element)

newWUF :: (Ix i, MArray arr i IO, MArray arr Int IO) => (i, i) -> i -> IO (WeightedUnionFind arr i)
newWUF (l, u) rep =
  WeightedUnionFind
    <$> newArray (l, u) rep
    <*> newArray (l, u) 1
    <*> newArray (l, u) 0
    <*> newIORef (bool 0 (ix u + 1 - ix l) (u >= l))
    <*> pure rep
  where
    ix = index (l, u)

rootWUF :: (Ix i, MArray arr i IO, MArray arr Int IO) => WeightedUnionFind arr i -> i -> IO i
rootWUF uf@(WeightedUnionFind parent _ weight _ rep) x = do
  p <- readArray parent x
  if p == rep
    then return x
    else do
      r <- rootWUF uf p
      writeArray parent x r
      -- 累積和
      w <- readArray weight p
      modifyArray weight x (+ w)
      return r

getWeightWUF :: (Ix i, MArray arr i IO, MArray arr Int IO) => WeightedUnionFind arr i -> i -> IO Int
getWeightWUF uf@(WeightedUnionFind _ _ weight _ _) x = do
  _ <- rootWUF uf x -- 経路圧縮
  readArray weight x

uniteWUF :: (Ix i, MArray arr i IO, MArray arr Int IO) => WeightedUnionFind arr i -> i -> i -> Int -> IO ()
uniteWUF uf@(WeightedUnionFind parent size weight refN _) x y w = do
  x' <- rootWUF uf x
  y' <- rootWUF uf y
  wx <- getWeightWUF uf x
  wy <- getWeightWUF uf y
  let w' = w + wx - wy
  when (x' /= y') $ do
    sizeX <- readArray size x'
    sizeY <- readArray size y'
    -- 併合する毎に一つ連結成分数が減る
    modifyIORef' refN (+ (-1))
    if sizeX > sizeY
      then do
        writeArray size x' (sizeX + sizeY)
        writeArray parent y' x'
        writeArray weight y' w'
      else do
        writeArray size y' (sizeX + sizeY)
        writeArray parent x' y'
        writeArray weight x' (negate w')

isSameWUF :: (Ix i, MArray arr i IO, MArray arr Int IO) => WeightedUnionFind arr i -> i -> i -> IO Bool
isSameWUF uf x y = (==) <$> rootWUF uf x <*> rootWUF uf y
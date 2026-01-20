module UnionFindSTUArray where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST (STUArray, newArray, readArray, writeArray)

-- UnionFind データ構造 (ST版)
data UnionFind s = UnionFind (STUArray s Int Int) (STUArray s Int Int)

newUF :: (Int, Int) -> ST s (UnionFind s)
newUF (s, e) = UnionFind <$> newArray (s, e) (-1) <*> newArray (s, e) 1

getRoot :: UnionFind s -> Int -> ST s Int
getRoot uf@(UnionFind parent _) x = do
  p <- readArray parent x
  if p == (-1)
    then return x
    else do
      p' <- getRoot uf p
      writeArray parent x p' -- 経路圧縮
      return p'

-- union by size
unite :: UnionFind s -> Int -> Int -> ST s ()
unite uf@(UnionFind parent size) x y = do
  x' <- getRoot uf x
  y' <- getRoot uf y
  when (x' /= y') $ do
    sizeX <- readArray size x'
    sizeY <- readArray size y'
    if sizeX > sizeY
      then do
        writeArray parent y' x'
        writeArray size x' (sizeX + sizeY)
      else do
        writeArray parent x' y'
        writeArray size y' (sizeX + sizeY)

-- xとyが同じ連結成分に属するか
sameUF :: UnionFind s -> Int -> Int -> ST s Bool
sameUF uf x y = (==) <$> getRoot uf x <*> getRoot uf y

getSize :: UnionFind s -> Int -> ST s Int
getSize uf@(UnionFind _ size) u = do
  p <- getRoot uf u
  readArray size p

getEdgeCount :: UnionFind s -> Int -> ST s Int
getEdgeCount uf u = do
  s <- getSize uf u
  return $ s * (s - 1) `quot` 2
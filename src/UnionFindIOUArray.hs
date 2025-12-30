module UnionFindIOUArray where

import Control.Monad (foldM, replicateM, unless, when)
import Control.Monad.ST ()
import Data.Array.IO (IOUArray, MArray (newArray), readArray, writeArray)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import Data.Foldable (forM_)
import Data.List

{-- IOUArray版 --}
-- UnionFind 各ノードの親ノード 各ノードが属する木のsize
-- ノード番号をuniqなintとして持っておく
-- arrayを使って、各ノードに対応する親ノード、sizeが取得できるようにする
-- UnionFind (IOUArray lowerbound upperbound) (IOUArray parent size)
data UnionFind = UnionFind (IOUArray Int Int) (IOUArray Int Int)

newUF :: (Int, Int) -> IO UnionFind
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b を適用
-- IO (a -> b) -> IO a -> IO b となる
newUF (s, e) = UnionFind <$> newArray (s, e) (-1) <*> newArray (s, e) 1

getRoot :: UnionFind -> Int -> IO Int
getRoot uf@(UnionFind parent _) x = do
  p <- readArray parent x
  if p == (-1)
    then return x
    else do
      p' <- getRoot uf p
      writeArray parent x p' -- 経路圧縮
      return p'

-- union by size
unit :: UnionFind -> Int -> Int -> IO ()
unit uf@(UnionFind parent size) x y = do
  same <- sameUF uf x y
  when same $ return ()

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

-- xとyが同じUFである(閉路をつくる)場合にtrueを返す
sameUF :: UnionFind -> Int -> Int -> IO Bool
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b を適用
-- IO (Int -> Bool) -> IO Int -> IO Bool となる
-- IO (== (root uf x)) <*> IO (root uf y) という形になる
sameUF uf x y = (==) <$> getRoot uf x <*> getRoot uf y

getSize :: UnionFind -> Int -> IO Int
getSize uf@(UnionFind _ size) u = do
  p <- getRoot uf u
  readArray size p

getEdgeCount :: UnionFind -> Int -> IO Int
getEdgeCount uf@(UnionFind _ size) u = do
  s <- getSize uf u
  return $ s * (s - 1) `quot` 2

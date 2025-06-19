{-# LANGUAGE FlexibleContexts #-}

{-- MVector版 --}
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Mutable as MV

-- UnionFindデータ構造
data UnionFind s = UnionFind
  { parent :: MV.MVector s Int, -- 親ノードを保持
    rank :: MV.MVector s Int -- ランク（木の高さ）を保持
  }

-- UnionFindの初期化
newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = do
  p <- MV.generate n id -- 各ノードは自分自身を親に設定
  r <- MV.replicate n 0 -- 初期ランクはすべて0
  return $ UnionFind p r

-- Find操作：経路圧縮を伴う
find :: UnionFind s -> Int -> ST s Int
find uf x = do
  px <- MV.read (parent uf) x
  if px /= x
    then do
      root <- find uf px -- 再帰的に親を探索
      MV.write (parent uf) x root -- 経路圧縮
      return root
    else return x

-- Union操作：ランク付き併合を伴う
union :: UnionFind s -> Int -> Int -> ST s ()
union uf x y = do
  rootX <- find uf x
  rootY <- find uf y
  when (rootX /= rootY) $ do
    rankX <- MV.read (rank uf) rootX
    rankY <- MV.read (rank uf) rootY
    if rankX < rankY
      then MV.write (parent uf) rootX rootY
      else
        if rankX > rankY
          then MV.write (parent uf) rootY rootX
          else do
            MV.write (parent uf) rootY rootX
            MV.modify (rank uf) (+ 1) rootX

-- 同じ集合に属しているか判定
same :: UnionFind s -> Int -> Int -> ST s Bool
same uf x y = do
  rootX <- find uf x
  rootY <- find uf y
  return (rootX == rootY)

-- 使用例
main :: IO ()
main = do
  let n = 10
  let result = runST $ do
        uf <- newUnionFind n
        union uf 1 2
        union uf 2 3
        union uf 4 5
        same12 <- same uf 1 3
        same34 <- same uf 3 4
        return (same12, same34)
  print result

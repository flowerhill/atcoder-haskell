{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module WUnionFind where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Array.Base (MArray (..), readArray, writeArray)
import Data.Array.MArray (newListArray)
import Data.Ix (Ix, range, rangeSize)
import Data.Primitive.MutVar (MutVar, modifyMutVar', newMutVar, readMutVar)

-- $setup
-- >>> import Data.Array.IO (IOUArray)
-- >>> import Data.Array.ST (STUArray, runSTUArray)
-- >>> import Control.Monad.ST (runST)

-- | modifyArray の互換実装 (array < 0.5.6.0 向け)
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

{- 重み付き Union-Find (符号付き)
   ref: https://qiita.com/drken/items/cce6fc5c579051e64fab

   各要素 x について  A_x = sign[x] * A_root + offset[x]  (sign は ±1) を保持する。
-}
data WeightedUnionFind arr s i = WeightedUnionFind
  { wufParent :: arr i i, -- 親頂点 (parent[x] == x なら x が根)
    wufSize :: arr i Int, -- 集合サイズ (根で参照)
    wufSign :: arr i Int, -- 根に対する符号 (±1)
    wufOff :: arr i Int, -- 根に対するオフセット
    wufNComp :: MutVar s Int -- 連結成分数
  }

-- | 範囲 (l, u) で重み付き Union-Find を初期化する
--
-- IO で使う例:
--
-- >>> uf <- newWUF @IOUArray (1, 5)
-- >>> rootWUF uf 3
-- 3
-- >>> countWUF uf
-- 5
--
-- ST で使う例:
--
-- >>> :{
-- runST $ do
--   uf <- newWUF @(STUArray _) (1, 5)
--   uniteDiffWUF uf 1 2 3
--   countWUF uf
-- :}
-- 4
newWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  (i, i) ->
  m (WeightedUnionFind arr (PrimState m) i)
newWUF bnds = do
  par <- newListArray bnds (range bnds)
  sz <- newArray bnds 1
  sg <- newArray bnds 1
  off <- newArray bnds 0
  nc <- newMutVar (rangeSize bnds)
  return $ WeightedUnionFind par sz sg off nc

-- | 経路圧縮付き find
-- 返り値 (root, s, o) は A_x = s * A_root + o を意味する
--
-- >>> uf <- newWUF @IOUArray (1, 3)
-- >>> findWUF uf 2
-- (2,1,0)
-- >>> uniteDiffWUF uf 1 2 5
-- >>> findWUF uf 2
-- (1,1,5)
findWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  m (i, Int, Int)
findWUF uf x = do
  p <- readArray (wufParent uf) x
  if p == x
    then return (x, 1, 0)
    else do
      (r, sp, op) <- findWUF uf p
      sx <- readArray (wufSign uf) x
      ox <- readArray (wufOff uf) x
      let !ns = sx * sp
          !no = sx * op + ox
      writeArray (wufParent uf) x r
      writeArray (wufSign uf) x ns
      writeArray (wufOff uf) x no
      return (r, ns, no)

-- | 要素の根を返す（経路圧縮あり）
--
-- >>> uf <- newWUF @IOUArray (1, 5)
-- >>> rootWUF uf 3
-- 3
-- >>> uniteDiffWUF uf 1 3 0
-- >>> rootWUF uf 3
-- 1
rootWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  m i
rootWUF uf x = do
  (r, _, _) <- findWUF uf x
  return r

-- | A_y = s * A_x + o (s は ±1) という関係を追加する
--
-- >>> uf <- newWUF @IOUArray (1, 3)
-- >>> uniteWUF uf 1 2 (-1) 7
-- >>> relationWUF uf 1 2
-- Just (-1,7)
uniteWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  i ->
  Int ->
  Int ->
  m ()
uniteWUF uf x y s o = do
  (rx, sx, ox) <- findWUF uf x
  (ry, sy, oy) <- findWUF uf y
  when (rx /= ry) do
    sizeX <- readArray (wufSize uf) rx
    sizeY <- readArray (wufSize uf) ry
    modifyMutVar' (wufNComp uf) (subtract 1)
    let !alpha = s * sx * sy
        !beta = sy * (s * ox + o - oy)
    if sizeX >= sizeY
      then do
        writeArray (wufParent uf) ry rx
        writeArray (wufSign uf) ry alpha
        writeArray (wufOff uf) ry beta
        writeArray (wufSize uf) rx (sizeX + sizeY)
      else do
        writeArray (wufParent uf) rx ry
        writeArray (wufSign uf) rx alpha
        writeArray (wufOff uf) rx (negate (alpha * beta))
        writeArray (wufSize uf) ry (sizeX + sizeY)

-- | A_y - A_x = w (典型的な差分制約)
--
-- >>> uf <- newWUF @IOUArray (1, 4)
-- >>> uniteDiffWUF uf 1 2 3
-- >>> uniteDiffWUF uf 2 3 5
-- >>> getWeightWUF uf 3
-- 8
uniteDiffWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  i ->
  Int ->
  m ()
uniteDiffWUF uf x y w = uniteWUF uf x y 1 w
{-# INLINE uniteDiffWUF #-}

-- | A_x + A_y = v (符号反転を含む和の制約)
--
-- >>> uf <- newWUF @IOUArray (1, 3)
-- >>> uniteSumWUF uf 1 2 10
-- >>> relationWUF uf 1 2
-- Just (-1,10)
uniteSumWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  i ->
  Int ->
  m ()
uniteSumWUF uf x y v = uniteWUF uf x y (-1) v
{-# INLINE uniteSumWUF #-}

-- | 2要素が同じ集合に属するか判定する
--
-- >>> uf <- newWUF @IOUArray (1, 3)
-- >>> uniteDiffWUF uf 1 2 1
-- >>> isSameWUF uf 1 2
-- True
-- >>> isSameWUF uf 1 3
-- False
isSameWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  i ->
  m Bool
isSameWUF uf x y = (==) <$> rootWUF uf x <*> rootWUF uf y

-- | 2要素の関係 A_y = s * A_x + o を取得する
--
-- >>> uf <- newWUF @IOUArray (1, 3)
-- >>> uniteDiffWUF uf 1 2 5
-- >>> relationWUF uf 1 2
-- Just (1,5)
-- >>> relationWUF uf 2 1
-- Just (1,-5)
-- >>> relationWUF uf 1 3
-- Nothing
relationWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  i ->
  m (Maybe (Int, Int))
relationWUF uf x y = do
  (rx, sx, ox) <- findWUF uf x
  (ry, sy, oy) <- findWUF uf y
  if rx /= ry
    then return Nothing
    else do
      let !s = sx * sy
          !o = oy - s * ox
      return $ Just (s, o)

-- | 要素の根に対するオフセットを返す（差分制約のみで使う場合は意味あり）
--
-- >>> uf <- newWUF @IOUArray (1, 4)
-- >>> uniteDiffWUF uf 1 2 3
-- >>> uniteDiffWUF uf 2 3 5
-- >>> getWeightWUF uf 3
-- 8
getWeightWUF ::
  forall arr m i.
  (PrimMonad m, Ix i, MArray arr i m, MArray arr Int m) =>
  WeightedUnionFind arr (PrimState m) i ->
  i ->
  m Int
getWeightWUF uf x = do
  (_, _, o) <- findWUF uf x
  return o

-- | 連結成分数を返す
--
-- >>> uf <- newWUF @IOUArray (1, 5)
-- >>> countWUF uf
-- 5
-- >>> uniteDiffWUF uf 1 2 0
-- >>> countWUF uf
-- 4
countWUF ::
  (PrimMonad m) =>
  WeightedUnionFind arr (PrimState m) i ->
  m Int
countWUF uf = readMutVar (wufNComp uf)

module CumSum where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.IArray (bounds, elems, listArray, (!))
import Data.Array.ST (STUArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.List (scanl')
import qualified Data.Vector.Unboxed as VU
import MyMArray (modifyArray2)

-- 方針:
--   累積和（前計算して区間和を O(1) で取る）と、
--   その逆向きの操作であるいもす法（差分を置いてから累積和を取る）をまとめて置く。

{-- 1 次元累積和 --}

-- | 累積和配列を作る。添字 i の値は先頭 i 要素の和で、bounds は (0, length xs)。
--
-- >>> import Data.Array.IArray (elems)
-- >>> elems (cumsumArray [3, 1, 4, 1, 5])
-- [0,3,4,8,9,14]
-- >>> elems (cumsumArray [])
-- [0]
cumsumArray :: [Int] -> UArray Int Int
cumsumArray xs = listArray (0, length xs) (scanl' (+) 0 xs)

-- | 累積和配列から 1-indexed の閉区間 [l, r] の和を取り出す
--
-- >>> rangeSum (cumsumArray [3, 1, 4, 1, 5]) 2 4
-- 6
-- >>> rangeSum (cumsumArray [3, 1, 4, 1, 5]) 1 5
-- 14
-- >>> rangeSum (cumsumArray [3, 1, 4, 1, 5]) 3 3
-- 4
rangeSum :: UArray Int Int -> Int -> Int -> Int
rangeSum cs l r = cs ! r - cs ! (l - 1)

{-- 2 次元累積和 --}

-- | 2次元累積和を計算する。O(H*W)
--
-- 行を Vector の slice で切り出し、行内で横方向に scanl1、
-- 直前の行の結果を zipWith (+) で足し込んで縦方向にも累積する。
-- 行幅は bounds の列方向の幅から取るので 0 始まりでも 1 始まりでも通る。
--
-- >>> import Data.Array.Unboxed (listArray, (!))
-- >>> let arr = listArray ((1,1),(2,2)) [1,2,3,4] :: UArray (Int,Int) Int
-- >>> twoDimensionalSum arr ! (2,2)
-- 10
-- >>> let arr0 = listArray ((0,0),(1,1)) [1,2,3,4] :: UArray (Int,Int) Int
-- >>> [twoDimensionalSum arr0 ! (i,j) | i <- [0..1], j <- [0..1]]
-- [1,3,4,10]
twoDimensionalSum :: UArray (Int, Int) Int -> UArray (Int, Int) Int
twoDimensionalSum arr = listArray bounds_ $ VU.toList $ VU.concat rowSums
  where
    bounds_@((r0, c0), (r1, c1)) = bounds arr
    h = r1 - r0 + 1
    w = c1 - c0 + 1
    src = VU.fromList $ elems arr
    rows = [VU.slice (i * w) w src | i <- [0 .. h - 1]]
    rowSums = scanl1 (VU.zipWith (+)) $ map (VU.scanl1 (+)) rows

{-- いもす法 --}

-- | 1次元いもす法: 閉区間 [l, r] への加算クエリを処理し、
-- 各点での合計値の UArray を返す。
-- bounds は結果配列の範囲。各クエリの r は bounds の上限まで指定可。
-- O(N + W)、N=クエリ数, W=範囲幅。
--
-- >>> import Data.Array.Unboxed (elems, (!))
-- >>> elems $ imos1D (0, 4) [(0, 2, 1), (1, 3, 2)]
-- [1,3,3,2,0]
-- >>> elems $ imos1D (0, 3) []
-- [0,0,0,0]
-- >>> imos1D (1, 5) [(2, 5, 10)] ! 5
-- 10
imos1D :: (Int, Int) -> [(Int, Int, Int)] -> UArray Int Int
imos1D (lo, hi) qs = runSTUArray $ do
  -- 閉区間なので r+1 に書き込める必要があり、bounds は (lo, hi+1)
  diff <- newArray (lo, hi + 1) 0 :: ST s (STUArray s Int Int)
  forM_ qs $ \(l, r, v) -> do
    modifyArray2 diff l v (+)
    modifyArray2 diff (r + 1) (negate v) (+) -- 閉区間 [l, r] = r の「次」で打ち消す
    -- 累積和
  forM_ [lo + 1 .. hi] $ \i -> do
    prev <- readArray diff (i - 1)
    modifyArray2 diff i prev (+)
  -- 結果配列に lo..hi だけコピーして返す
  result <- newArray (lo, hi) 0 :: ST s (STUArray s Int Int)
  forM_ [lo .. hi] $ \i -> do
    v <- readArray diff i
    writeArray result i v
  return result

-- | 2次元いもす法: 閉矩形 [lx, rx] × [ly, ry] への加算クエリを処理し、
-- 各単位セル (x, y) での合計値の UArray を返す。
-- bounds は結果配列のセル範囲。各クエリの rx, ry は bounds の上限まで指定可。
-- O(N + W*H)。
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> let arr = imos2D ((0,0),(2,2)) [((0,0),(1,1),1), ((1,1),(2,2),1)]
-- >>> [arr ! (i,j) | i <- [0..2], j <- [0..2]]
-- [1,1,0,1,2,1,0,1,1]
-- >>> let arr = imos2D ((0,0),(1,1)) [((0,0),(1,1),5)]
-- >>> [arr ! (i,j) | i <- [0..1], j <- [0..1]]
-- [5,5,5,5]
imos2D ::
  ((Int, Int), (Int, Int)) ->
  [((Int, Int), (Int, Int), Int)] ->
  UArray (Int, Int) Int
imos2D ((xlo, ylo), (xhi, yhi)) qs = runSTUArray $ do
  -- 閉矩形なので (rx+1, ry+1) に書き込める必要があり、余白 +1
  let dbnd = ((xlo, ylo), (xhi + 1, yhi + 1))
  diff <- newArray dbnd 0 :: ST s (STUArray s (Int, Int) Int)
  forM_ qs $ \((lx, ly), (rx, ry), v) -> do
    modifyArray2 diff (lx, ly) v (+)
    modifyArray2 diff (rx + 1, ly) (negate v) (+)
    modifyArray2 diff (lx, ry + 1) (negate v) (+)
    modifyArray2 diff (rx + 1, ry + 1) v (+)
  -- x方向 累積和
  forM_ [ylo .. yhi + 1] $ \y ->
    forM_ [xlo + 1 .. xhi + 1] $ \x -> do
      prev <- readArray diff (x - 1, y)
      modifyArray2 diff (x, y) prev (+)
  -- y方向 累積和
  forM_ [xlo .. xhi + 1] $ \x ->
    forM_ [ylo + 1 .. yhi + 1] $ \y -> do
      prev <- readArray diff (x, y - 1)
      modifyArray2 diff (x, y) prev (+)
  -- セル範囲だけ結果に詰め直す
  result <- newArray ((xlo, ylo), (xhi, yhi)) 0 :: ST s (STUArray s (Int, Int) Int)
  forM_ [xlo .. xhi] $ \x ->
    forM_ [ylo .. yhi] $ \y -> do
      v <- readArray diff (x, y)
      writeArray result (x, y) v
  return result

-- | 2次元いもす法（半開矩形版）: 半開矩形 [lx, rx) × [ly, ry) への加算クエリを処理し、
-- 各単位マス (x, y) = [x, x+1) × [y, y+1) での合計値の UArray を返す。
-- 引数はマスの番号ではなく矩形の「辺の座標」で、bounds もその座標の範囲を指定する。
-- マス (xhi, *) と (*, yhi) は辺より右上に出るため常に 0 になる。
-- 閉区間版の imos2D と違い、打ち消しを rx, ry にそのまま置けるので
-- 呼び出し側で ±1 の変換が要らず、余白確保と結果のコピーも不要。O(N + W*H)。
--
-- マスの番号を直接指定したい場合は閉区間版の imos2D を使う。
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> let arr = imos2DHalfOpen ((0,0),(3,3)) [((0,0),(2,2),1), ((1,1),(3,3),1)]
-- >>> [arr ! (i,j) | i <- [0..2], j <- [0..2]]
-- [1,1,0,1,2,1,0,1,1]
--
-- 辺だけを共有する2つの矩形は重ならない（半開区間なので rx がそのまま境界）
-- >>> let arr = imos2DHalfOpen ((0,0),(2,1)) [((0,0),(1,1),1), ((1,0),(2,1),1)]
-- >>> [arr ! (i,0) | i <- [0..1]]
-- [1,1]
--
-- 辺の上限に接するマスは範囲外なので 0 のまま
-- >>> let arr = imos2DHalfOpen ((0,0),(2,2)) [((0,0),(1,1),5)]
-- >>> [arr ! (i,j) | i <- [0..2], j <- [0..2]]
-- [5,0,0,0,0,0,0,0,0]
imos2DHalfOpen ::
  ((Int, Int), (Int, Int)) ->
  [((Int, Int), (Int, Int), Int)] ->
  UArray (Int, Int) Int
imos2DHalfOpen bnd@((xlo, ylo), (xhi, yhi)) qs = runSTUArray $ do
  diff <- newArray bnd 0 :: ST s (STUArray s (Int, Int) Int)
  forM_ qs $ \((lx, ly), (rx, ry), v) -> do
    modifyArray2 diff (lx, ly) v (+)
    modifyArray2 diff (rx, ly) (negate v) (+) -- 半開区間 [lx, rx) なので rx で打ち消す
    modifyArray2 diff (lx, ry) (negate v) (+)
    modifyArray2 diff (rx, ry) v (+)
  -- x方向 累積和
  forM_ [ylo .. yhi] $ \y ->
    forM_ [xlo + 1 .. xhi] $ \x -> do
      prev <- readArray diff (x - 1, y)
      modifyArray2 diff (x, y) prev (+)
  -- y方向 累積和
  forM_ [xlo .. xhi] $ \x ->
    forM_ [ylo + 1 .. yhi] $ \y -> do
      prev <- readArray diff (x, y - 1)
      modifyArray2 diff (x, y) prev (+)
  return diff

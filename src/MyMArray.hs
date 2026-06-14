{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module MyMArray where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.IArray (IArray (bounds), elems, listArray)
import Data.Array.IO
import Data.Array.ST (STUArray)
import Data.Array.ST.Safe (runSTUArray)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import qualified Data.List.Split as LS

{-- MArray --}

-- | MArray の i 番目と j 番目の要素を入れ替える
--
-- >>> import Data.Array.ST (thaw, runSTUArray); import Data.Array.Unboxed ((!), listArray, UArray)
-- >>> let src = listArray (0,2) [1,2,3] :: UArray Int Int
-- >>> runSTUArray (thaw src >>= \a -> swapArray a 0 2 >> return a) ! 0
-- 3
swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray as i j = do
  a <- readArray as i
  b <- readArray as j
  writeArray as j $! a
  writeArray as i $! b
{-# INLINE swapArray #-}

-- | 2引数の更新関数で MArray の要素を更新する
--
-- >>> import Data.Array.ST (thaw, runSTUArray); import Data.Array.Unboxed ((!), listArray, UArray)
-- >>> let src = listArray (0,2) [1,2,3] :: UArray Int Int
-- >>> runSTUArray (thaw src >>= \a -> modifyArray2 a 1 10 (+) >> return a) ! 1
-- 12
modifyArray2 :: (MArray a e m, Ix i) => a i e -> i -> e' -> (e -> e' -> e) -> m ()
modifyArray2 arr ix x f = do
  v <- readArray arr ix
  writeArray arr ix $! f v x
{-# INLINE modifyArray2 #-}

{-- IOArray --}
-- n: 抽出したい行番号
-- w: 列の最大インデックス
getRowAsIOArray :: Int -> Int -> IOUArray (Int, Int) Int -> IO (IOUArray Int Int)
getRowAsIOArray n w arr = do
  -- 新しい一次元配列を作成
  newArr <- newArray (0, w) minBound :: IO (IOUArray Int Int)
  -- 元の配列から値を読み取り、新しい配列に書き込む
  mapM_
    ( \col -> do
        val <- readArray arr (n, col)
        writeArray newArr col val
    )
    [0 .. w]
  return newArr

{-- STUArray --}
-- ST版
-- n: 抽出したい行番号
-- w: 列の最大インデックス
getRowAsSTUArray :: (MArray a1 Int (ST s), Ix a2) => a2 -> Int -> a1 (a2, Int) Int -> ST s (STUArray s Int Int)
getRowAsSTUArray n w arr = do
  -- 新しい一次元配列を作成
  newArr <- newArray (0, w) minBound :: ST s (STUArray s Int Int)
  -- 元の配列から値を読み取り、新しい配列に書き込む
  mapM_
    ( \col -> do
        val <- readArray arr (n, col)
        writeArray newArr col val
    )
    [0 .. w]
  return newArr

{-- grid --}

-- | UArray の行列を表示する（IO）
printMatrix :: UArray (Int, Int) Int -> IO ()
printMatrix arr = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
  let rows = [[arr UA.! (row, col) | col <- [minCol .. maxCol - 1]] | row <- [minRow .. maxRow - 1]]
  putStr $ unlines [unwords (map show row) | row <- rows]

-- | 2次元累積和を計算する（1始まりインデックス対応）
--
-- >>> import Data.Array.Unboxed (listArray, (!))
-- >>> let arr = listArray ((1,1),(2,2)) [1,2,3,4] :: UArray (Int,Int) Int
-- >>> twoDimensionalSum arr ! (2,2)
-- 10
twoDimensionalSum :: UArray (Int, Int) Int -> UArray (Int, Int) Int
twoDimensionalSum arr =
  listArray bounds_ $
    concat $
      scanl1 (zipWith (+)) $
        map (scanl1 (+)) lists
  where
    bounds_ = bounds arr
    lists = LS.chunksOf ((snd . snd) bounds_) $ elems arr

{-- MArray用 --}

-- | 範囲内なら Just、範囲外なら Nothing を返す（モナド版）
--
-- >>> import Data.Array.IO (IOUArray, newListArray)
-- >>> a <- newListArray (0,2) [1,2,3 :: Int] :: IO (IOUArray Int Int)
-- >>> a !? 1
-- Just 2
-- >>> a !? 5
-- Nothing
(!?) :: (Ix i, MArray a e m) => a i e -> i -> m (Maybe e)
(!?) arr idx = do
  bounds' <- getBounds arr
  if inRange bounds' idx
    then do
      val <- readArray arr idx
      return (Just val)
    else return Nothing

-- | 範囲内なら書き込んで True、範囲外なら False を返す
--
-- >>> import Data.Array.IO (IOUArray, newListArray)
-- >>> a <- newListArray (0,2) [1,2,3 :: Int] :: IO (IOUArray Int Int)
-- >>> r <- safeWriteArray a 1 99
-- >>> v <- readArray a 1
-- >>> (r, v)
-- (True,99)
-- >>> safeWriteArray a 5 99
-- False
safeWriteArray :: (Ix i, MArray a e m) => a i e -> i -> e -> m Bool
safeWriteArray arr idx val = do
  bounds' <- getBounds arr
  if inRange bounds' idx
    then do
      writeArray arr idx val
      return True
    else return False

-- | 範囲内なら Just で値を返す安全な読み取り（モナド版）
--
-- >>> import Data.Array.IO (IOUArray, newListArray)
-- >>> a <- newListArray (0,2) [10,20,30 :: Int] :: IO (IOUArray Int Int)
-- >>> safeReadArray a 1
-- Just 20
-- >>> safeReadArray a 5
-- Nothing
safeReadArray :: (Ix i, MArray a e m) => a i e -> i -> m (Maybe e)
safeReadArray arr idx = do
  bounds' <- getBounds arr
  if inRange bounds' idx
    then Just <$> readArray arr idx
    else return Nothing

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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module MyMArray where

import BSearchVector (bisectM)
import Control.Monad (foldM)
import Control.Monad.ST
import Data.Array.IArray (IArray (bounds), elems, listArray)
import Data.Array.IO
import Data.Array.ST (STUArray, runSTUArray)
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

{-- LIS / LDS --}

-- | 各位置 i で終わる最長増加部分列（狭義）の長さを返す (O(N log N))。
-- patience sorting の tails を STUArray で持ち、各要素で lower bound を二分探索する。
-- 現在長 len は foldM で引き回すので可変セルは不要。
--
-- >>> elems (lisLengths [3,1,4,1,5,9,2,6])
-- [1,1,2,1,3,4,2,4]
-- >>> elems (lisLengths [5,4,3,2,1])
-- [1,1,1,1,1]
lisLengths :: [Int] -> UArray Int Int
lisLengths xs = runSTUArray $ do
  tails <- newArray (0, n - 1) maxBound :: ST s (STUArray s Int Int)
  result <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Int)
  let step len (i, x) = do
        -- tails[0..len) のうち x 以上が初めて現れる位置 pos（無ければ len）
        (_, pos) <- bisectM (-1, len) $ \mid -> (>= x) <$> readArray tails mid
        writeArray tails pos x
        writeArray result i (pos + 1)
        return $ max len (pos + 1)
  _ <- foldM step 0 (zip [0 ..] xs)
  return result
  where
    n = length xs

-- | 各位置 i から始まる最長減少部分列（狭義）の長さを返す (O(N log N))。
-- 反転して LIS を取り、結果を反転して戻す。
--
-- >>> elems (ldsLengths [3,1,4,1,5,9,2,6])
-- [2,1,2,1,2,2,1,1]
ldsLengths :: [Int] -> UArray Int Int
ldsLengths xs = listArray (0, n - 1) . reverse . elems . lisLengths $ reverse xs
  where
    n = length xs

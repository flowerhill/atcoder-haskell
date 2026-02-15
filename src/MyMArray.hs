{-# LANGUAGE FlexibleContexts #-}

module MyMArray where

import Control.Monad.ST
import Data.Array.IArray (IArray (bounds), elems, listArray)
import Data.Array.IO
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import qualified Data.List.Split as LS

{-- MArray --}

swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray as i j = do
  a <- readArray as i
  b <- readArray as j
  writeArray as j $! a
  writeArray as i $! b
{-# INLINE swapArray #-}

-- 関数の引数が2つある場合 更新用の関数を受けてarrayを更新する
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

printMatrix :: UArray (Int, Int) Int -> IO ()
printMatrix arr = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
  let rows = [[arr UA.! (row, col) | col <- [minCol .. maxCol - 1]] | row <- [minRow .. maxRow - 1]]
  putStr $ unlines [unwords (map show row) | row <- rows]

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
(!?) :: (Ix i, MArray a e m) => a i e -> i -> m (Maybe e)
(!?) arr idx = do
  bounds' <- getBounds arr
  if inRange bounds' idx
    then do
      val <- readArray arr idx
      return (Just val)
    else return Nothing

-- 安全な書き込み
safeWriteArray :: (Ix i, MArray a e m) => a i e -> i -> e -> m Bool
safeWriteArray arr idx val = do
  bounds' <- getBounds arr
  if inRange bounds' idx
    then do
      writeArray arr idx val
      return True
    else return False

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BSearchArray (module BSearch, module BSearchArray) where

import BSearch
import Data.Array.IArray (IArray, Ix, bounds, (!))

{-- 二分探索 --}

-- 命名: 末尾の A は IArray 版であることを表す（Vector 版は V）。
-- バンドラが全モジュールを 1 ファイルに展開するため、Array 版と Vector 版で名前を分けている。

-- | x以上の値が最初に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGEIdxA 4 arr
-- Just 2
-- >>> lookupGEIdxA 10 arr
-- Nothing
lookupGEIdxA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupGEIdxA x xs = do
  let (_, ub) = bounds xs
      i = boundGEA x xs

  if i == succ ub
    then Nothing
    else Just i

-- | xより大きい値が最初に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGTIdxA 5 arr
-- Just 3
-- >>> lookupGTIdxA 9 arr
-- Nothing
lookupGTIdxA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupGTIdxA x xs = do
  let (_, ub) = bounds xs
      i = boundGTA x xs

  if i == succ ub
    then Nothing
    else Just i

-- | xより小さい値が最後に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLTIdxA 5 arr
-- Just 1
-- >>> lookupLTIdxA 1 arr
-- Nothing
lookupLTIdxA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupLTIdxA x xs = do
  let (lb, _) = bounds xs
      i = boundLTA x xs

  if i == pred lb
    then Nothing
    else Just i

-- | x以下の値が最後に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLEIdxA 5 arr
-- Just 2
-- >>> lookupLEIdxA 0 arr
-- Nothing
lookupLEIdxA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupLEIdxA x xs = do
  let (lb, _) = bounds xs
      i = boundLEA x xs

  if i == pred lb
    then Nothing
    else Just i

-- | x以上の値が最初に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGEA 4 arr
-- Just 5
-- >>> lookupGEA 10 arr
-- Nothing
lookupGEA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGEA x xs = do
  let (_, ub) = bounds xs
      i = boundGEA x xs

  if i == succ ub
    then Nothing
    else Just (xs ! i)

-- | xより大きい値が最初に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGTA 5 arr
-- Just 7
-- >>> lookupGTA 9 arr
-- Nothing
lookupGTA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGTA x xs = do
  let (_, ub) = bounds xs
      i = boundGTA x xs

  if i == succ ub
    then Nothing
    else Just (xs ! i)

-- | xより小さい値が最後に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLTA 5 arr
-- Just 3
-- >>> lookupLTA 1 arr
-- Nothing
lookupLTA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLTA x xs = do
  let (lb, _) = bounds xs
      i = boundLTA x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

-- | x以下の値が最後に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLEA 5 arr
-- Just 5
-- >>> lookupLEA 0 arr
-- Nothing
lookupLEA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLEA x xs = do
  let (lb, _) = bounds xs
      i = boundLEA x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

-- | x以上の値が最初に現れるインデックスを取得（境界外の場合は succ ub を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundGEA 4 arr
-- 2
-- >>> boundGEA 10 arr
-- 5
boundGEA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGEA x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ok

-- | xより大きい値が最初に現れるインデックスを取得（境界外の場合は succ ub を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundGTA 5 arr
-- 3
-- >>> boundGTA 9 arr
-- 5
boundGTA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGTA x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ok

-- | xより小さい値が最後に現れるインデックスを取得（境界外の場合は pred lb を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundLTA 5 arr
-- 1
-- >>> boundLTA 1 arr
-- -1
boundLTA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLTA x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ng

-- | x以下の値が最後に現れるインデックスを取得（境界外の場合は pred lb を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundLEA 5 arr
-- 2
-- >>> boundLEA 0 arr
-- -1
boundLEA :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLEA x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ng
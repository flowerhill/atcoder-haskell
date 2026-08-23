{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BSearchArray (module BSearch, module BSearchArray) where

import BSearch
import Data.Array.IArray (IArray, Ix, bounds, (!))

{-- 二分探索 --}

-- | x以上の値が最初に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGEIdx 4 arr
-- Just 2
-- >>> lookupGEIdx 10 arr
-- Nothing
lookupGEIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupGEIdx x xs = do
  let (_, ub) = bounds xs
      i = boundGE x xs

  if i == succ ub
    then Nothing
    else Just i

-- | xより大きい値が最初に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGTIdx 5 arr
-- Just 3
-- >>> lookupGTIdx 9 arr
-- Nothing
lookupGTIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupGTIdx x xs = do
  let (_, ub) = bounds xs
      i = boundGT x xs

  if i == succ ub
    then Nothing
    else Just i

-- | xより小さい値が最後に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLTIdx 5 arr
-- Just 1
-- >>> lookupLTIdx 1 arr
-- Nothing
lookupLTIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupLTIdx x xs = do
  let (lb, _) = bounds xs
      i = boundLT x xs

  if i == pred lb
    then Nothing
    else Just i

-- | x以下の値が最後に現れるインデックスを取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLEIdx 5 arr
-- Just 2
-- >>> lookupLEIdx 0 arr
-- Nothing
lookupLEIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupLEIdx x xs = do
  let (lb, _) = bounds xs
      i = boundLE x xs

  if i == pred lb
    then Nothing
    else Just i

-- | x以上の値が最初に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGE 4 arr
-- Just 5
-- >>> lookupGE 10 arr
-- Nothing
lookupGE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGE x xs = do
  let (_, ub) = bounds xs
      i = boundGE x xs

  if i == succ ub
    then Nothing
    else Just (xs ! i)

-- | xより大きい値が最初に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupGT 5 arr
-- Just 7
-- >>> lookupGT 9 arr
-- Nothing
lookupGT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGT x xs = do
  let (_, ub) = bounds xs
      i = boundGT x xs

  if i == succ ub
    then Nothing
    else Just (xs ! i)

-- | xより小さい値が最後に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLT 5 arr
-- Just 3
-- >>> lookupLT 1 arr
-- Nothing
lookupLT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLT x xs = do
  let (lb, _) = bounds xs
      i = boundLT x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

-- | x以下の値が最後に現れる値を取得
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> lookupLE 5 arr
-- Just 5
-- >>> lookupLE 0 arr
-- Nothing
lookupLE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLE x xs = do
  let (lb, _) = bounds xs
      i = boundLE x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

-- | x以上の値が最初に現れるインデックスを取得（境界外の場合は succ ub を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundGE 4 arr
-- 2
-- >>> boundGE 10 arr
-- 5
boundGE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGE x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ok

-- | xより大きい値が最初に現れるインデックスを取得（境界外の場合は succ ub を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundGT 5 arr
-- 3
-- >>> boundGT 9 arr
-- 5
boundGT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGT x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ok

-- | xより小さい値が最後に現れるインデックスを取得（境界外の場合は pred lb を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundLT 5 arr
-- 1
-- >>> boundLT 1 arr
-- -1
boundLT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLT x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ng

-- | x以下の値が最後に現れるインデックスを取得（境界外の場合は pred lb を返す）
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1,3,5,7,9] :: Data.Array.Array Int Int
-- >>> boundLE 5 arr
-- 2
-- >>> boundLE 0 arr
-- -1
boundLE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLE x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ng
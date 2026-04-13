{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BSearchArray where

import Data.Array.IArray (IArray, Ix, bounds, (!))

{-- 二分探索 --}

-- | 左が true / 右が false で境界を引く
-- ok で f が True、ng で f が False となる境界を返す
--
-- >>> bisect2 (0, 10) (\x -> x <= 5)
-- (5,6)
-- >>> bisect2 (10, 0) (\x -> x >= 3)
-- (3,2)
bisect2 :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect2 (ok, ng) f
  | abs (ng - ok) == 1 = (ok, ng)
  | f m = bisect2 (m, ng) f
  | otherwise = bisect2 (ok, m) f
  where
    m = (ok + ng) `div` 2

-- | 左が false / 右が true で境界を引く
-- ng で f が False、ok で f が True となる境界を返す
--
-- >>> bisect (-1, 10) (\x -> x >= 3)
-- (2,3)
-- >>> bisect (0, 100) (\x -> x * x >= 25)
-- (4,5)
bisect :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect (ng, ok) f
  | abs (ok - ng) == 1 = (ng, ok)
  | f m = bisect (ng, m) f
  | otherwise = bisect (m, ok) f
  where
    m = (ok + ng) `div` 2

-- | モナド版二分探索
--
-- >>> bisectM (-1, 10) (\x -> return (x >= 3))
-- (2,3)
bisectM :: (Monad m, Integral a) => (a, a) -> (a -> m Bool) -> m (a, a)
bisectM (ng, ok) f
  | abs (ok - ng) == 1 = return (ng, ok)
  | otherwise = do
      x <- f mid
      if x
        then bisectM (ng, mid) f
        else bisectM (mid, ok) f
  where
    mid = (ok + ng) `div` 2

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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BSearchArray where

import Data.Array.IArray (IArray, Ix, bounds, (!))

{-- 二分探索 --}

-- 左が true / 右が false で境界を引く
bisect2 :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect2 (ok, ng) f
  | abs (ng - ok) == 1 = (ok, ng)
  | f m = bisect2 (m, ng) f
  | otherwise = bisect2 (ok, m) f
  where
    m = (ok + ng) `div` 2

-- | 左が false / 右が true で境界を引く
bisect :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect (ng, ok) f
  | abs (ok - ng) == 1 = (ng, ok)
  | f m = bisect (ng, m) f
  | otherwise = bisect (m, ok) f
  where
    m = (ok + ng) `div` 2

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

lookupGEIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupGEIdx x xs = do
  let (_, ub) = bounds xs
      i = boundGE x xs

  if i == succ ub
    then Nothing
    else Just i

lookupGTIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupGTIdx x xs = do
  let (_, ub) = bounds xs
      i = boundGT x xs

  if i == succ ub
    then Nothing
    else Just i

lookupLTIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupLTIdx x xs = do
  let (lb, _) = bounds xs
      i = boundLT x xs

  if i == pred lb
    then Nothing
    else Just i

lookupLEIdx :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe i
lookupLEIdx x xs = do
  let (lb, _) = bounds xs
      i = boundLE x xs

  if i == pred lb
    then Nothing
    else Just i

lookupGE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGE x xs = do
  let (_, ub) = bounds xs
      i = boundGE x xs

  if i == succ ub
    then Nothing
    else Just (xs ! i)

lookupGT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGT x xs = do
  let (_, ub) = bounds xs
      i = boundGT x xs

  if i == succ ub
    then Nothing
    else Just (xs ! i)

lookupLT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLT x xs = do
  let (lb, _) = bounds xs
      i = boundLT x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

lookupLE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLE x xs = do
  let (lb, _) = bounds xs
      i = boundLE x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

boundGE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGE x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ok

boundGT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGT x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ok

boundLT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLT x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ng

boundLE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLE x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ng
import Control.Monad
import Data.Array (Array)
import Data.Array.Base (UArray (UArray))
import Data.Array.IArray (accumArray, listArray, (!), bounds)
import Data.Array.IO.Internals (IOArray (IOArray), IOUArray (IOUArray))

{-- 二分探索 --}
-- bisect naoyaさんからパクったやつ

-- | 左が false / 右が true で境界を引く
bisect :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect (ng, ok) f
  | abs (ok - ng) == 1 = (ng, ok)
  | f m = bisect (ng, m) f
  | otherwise = bisect (m, ok) f
  where
    m = (ok + ng) `div` 2

-- | 左が true / 右が false で境界を引く
bisect2 :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect2 (ok, ng) f
  | abs (ng - ok) == 1 = (ok, ng)
  | f m = bisect2 (m, ng) f
  | otherwise = bisect2 (ok, m) f
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
    else i

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

-- haskboyさんからパクって改造したもの
upperBound arr val = ub
  where
    ub = case search binary (\x -> arr A.! x > val) ok ng of
      Just index -> index
      _ -> succ ok -- この設定のおかげでC++のインデクスのように振る舞える
    (ng, ok) = A.bounds arr

lowerBound arr val = lb
  where
    lb = case search binary (\x -> arr A.! x >= val) ok ng of
      Just index -> index
      _ -> succ ok -- この設定のおかげでC++のインデクスのように振る舞える
    (ng, ok) = A.bounds arr

-- Binary Search 2
search :: (a -> a -> Maybe a) -> (a -> Bool) -> a -> a -> Maybe a
search mid p ok ng
  | (not . p) ok = Nothing -- すべてNGの場合
  | p ng = Just ng -- すべてOKの場合
  | otherwise = go ok ng -- 途中でNG/OKが分かれる場合
  where
    go ok ng = case mid ok ng of
      Nothing -> Just ok
      Just m
        | p m -> go m ng
        | otherwise -> go ok m

-- 整数版
binary :: (Integral a) => a -> a -> Maybe a
binary l r
  | abs (r - l) > 1 = Just ((l + r) `div` 2)
  | otherwise = Nothing

-- 二分探索
-- 順方向にぶたん
lookupGT arr val = ub
  where
    ub = case search binary (\x -> arr IA.! x > val) ok ng of
      Just index -> index
      _ -> ok
    (ok, ng) = IA.bounds arr

lookupGE arr val = lb
  where
    lb = case search binary (\x -> arr IA.! x >= val) ok ng of
      Just index -> index
      _ -> ok
    (ok, ng) = IA.bounds arr

lookupLT arr val = ub
  where
    ub = case search binary (\x -> arr IA.! x < val) ok ng of
      Just index -> index
      _ -> ok
    (ok, ng) = IA.bounds arr

lookupLE arr val = ub
  where
    ub = case search binary (\x -> arr IA.! x <= val) ok ng of
      Just index -> index
      _ -> ok
    (ok, ng) = IA.bounds arr

-- 逆方向にぶたん
lookupGTR arr val = ub
  where
    ub = case search binary (\x -> arr IA.! x > val) ok ng of
      Just index -> index
      _ -> ok
    (ng, ok) = IA.bounds arr

lookupGER arr val = lb
  where
    lb = case search binary (\x -> arr IA.! x >= val) ok ng of
      Just index -> index
      _ -> ok
import Control.Monad
import Data.Array (Array)
import Data.Array.Base (UArray (UArray))
import Data.Array.IArray (accumArray, listArray, (!), bounds)
import Data.Array.IO.Internals (IOArray (IOArray), IOUArray (IOUArray))
import Data.Vector.Unboxed qualified as VU

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

-- 二分探索 Vector.Unboxed版

-- | 左が true / 右が false で境界を引く
bisect2 :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect2 (ok, ng) f
  | abs (ng - ok) == 1 = (ok, ng)
  | f m = bisect2 (m, ng) f
  | otherwise = bisect2 (ok, m) f
  where
    m = (ok + ng) `div` 2

-- | モナド版二分探索
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

-- | Vector用の二分探索（内部使用）
bisect :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect (ng, ok) f
  | abs (ok - ng) == 1 = (ng, ok)
  | f mid = bisect (ng, mid) f
  | otherwise = bisect (mid, ok) f
  where
    mid = (ok + ng) `div` 2

-- | x以上の値が最初に現れるインデックスを取得
lookupGEIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupGEIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundGE x xs
      in if i >= VU.length xs
           then Nothing
           else Just i

-- | xより大きい値が最初に現れるインデックスを取得
lookupGTIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupGTIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundGT x xs
      in if i >= VU.length xs
           then Nothing
           else Just i

-- | xより小さい値が最後に現れるインデックスを取得
lookupLTIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupLTIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundLT x xs
      in if i < 0
           then Nothing
           else Just i

-- | x以下の値が最後に現れるインデックスを取得
lookupLEIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupLEIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundLE x xs
      in if i < 0
           then Nothing
           else Just i

-- | x以上の値が最初に現れる値を取得
lookupGE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupGE x xs = do
  i <- lookupGEIdx x xs
  return (xs VU.! i)

-- | xより大きい値が最初に現れる値を取得
lookupGT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupGT x xs = do
  i <- lookupGTIdx x xs
  return (xs VU.! i)

-- | xより小さい値が最後に現れる値を取得
lookupLT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupLT x xs = do
  i <- lookupLTIdx x xs
  return (xs VU.! i)

-- | x以下の値が最後に現れる値を取得
lookupLE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupLE x xs = do
  i <- lookupLEIdx x xs
  return (xs VU.! i)

-- | x以上の値が最初に現れるインデックスを取得（境界外の場合は配列長を返す）
boundGE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundGE x xs
  | VU.null xs = 0
  | otherwise =
      let (_, ok) = bisect (-1, VU.length xs) (\i -> xs VU.! i >= x)
      in ok

-- | xより大きい値が最初に現れるインデックスを取得（境界外の場合は配列長を返す）
boundGT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundGT x xs
  | VU.null xs = 0
  | otherwise =
      let (_, ok) = bisect (-1, VU.length xs) (\i -> xs VU.! i > x)
      in ok

-- | xより小さい値が最後に現れるインデックスを取得（境界外の場合は-1を返す）
boundLT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundLT x xs
  | VU.null xs = -1
  | otherwise = 
      let (ng, _) = bisect (-1, VU.length xs) (\i -> xs VU.! i >= x)
      in ng

-- | x以下の値が最後に現れるインデックスを取得（境界外の場合は-1を返す）
boundLE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundLE x xs
  | VU.null xs = -1
  | otherwise = 
      let (ng, _) = bisect (-1, VU.length xs) (\i -> xs VU.! i > x)
      in ng

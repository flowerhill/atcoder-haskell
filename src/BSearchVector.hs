{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BSearchVector where

import Data.Vector.Unboxed qualified as VU

{-- 二分探索 Vector.Unboxed版 --}

-- | 左が false / 右が true で境界を引く
bisect :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect (ng, ok) f
  | abs (ok - ng) == 1 = (ng, ok)
  | f m = bisect (ng, m) f
  | otherwise = bisect (m, ok) f
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

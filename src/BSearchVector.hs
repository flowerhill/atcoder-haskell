{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BSearchVector where

import Data.Vector.Unboxed qualified as VU

{-- 二分探索 Vector.Unboxed版 --}

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
-- >>> lookupGEIdx 4 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 2
-- >>> lookupGEIdx 10 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGEIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupGEIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundGE x xs
       in if i >= VU.length xs
            then Nothing
            else Just i

-- | xより大きい値が最初に現れるインデックスを取得
--
-- >>> lookupGTIdx 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 3
-- >>> lookupGTIdx 9 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGTIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupGTIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundGT x xs
       in if i >= VU.length xs
            then Nothing
            else Just i

-- | xより小さい値が最後に現れるインデックスを取得
--
-- >>> lookupLTIdx 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 1
-- >>> lookupLTIdx 1 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLTIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupLTIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundLT x xs
       in if i < 0
            then Nothing
            else Just i

-- | x以下の値が最後に現れるインデックスを取得
--
-- >>> lookupLEIdx 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 2
-- >>> lookupLEIdx 0 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLEIdx :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupLEIdx x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundLE x xs
       in if i < 0
            then Nothing
            else Just i

-- | x以上の値が最初に現れる値を取得
--
-- >>> lookupGE 4 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 5
-- >>> lookupGE 10 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupGE x xs = do
  i <- lookupGEIdx x xs
  return (xs VU.! i)

-- | xより大きい値が最初に現れる値を取得
--
-- >>> lookupGT 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 7
-- >>> lookupGT 9 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupGT x xs = do
  i <- lookupGTIdx x xs
  return (xs VU.! i)

-- | xより小さい値が最後に現れる値を取得
--
-- >>> lookupLT 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 3
-- >>> lookupLT 1 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupLT x xs = do
  i <- lookupLTIdx x xs
  return (xs VU.! i)

-- | x以下の値が最後に現れる値を取得
--
-- >>> lookupLE 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 5
-- >>> lookupLE 0 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupLE x xs = do
  i <- lookupLEIdx x xs
  return (xs VU.! i)

-- | x以上の値が最初に現れるインデックスを取得（境界外の場合は配列長を返す）
--
-- >>> boundGE 4 (VU.fromList [1,3,5,7,9 :: Int])
-- 2
-- >>> boundGE 10 (VU.fromList [1,3,5,7,9 :: Int])
-- 5
boundGE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundGE x xs
  | VU.null xs = 0
  | otherwise =
      let (_, ok) = bisect (-1, VU.length xs) (\i -> xs VU.! i >= x)
       in ok

-- | xより大きい値が最初に現れるインデックスを取得（境界外の場合は配列長を返す）
--
-- >>> boundGT 5 (VU.fromList [1,3,5,7,9 :: Int])
-- 3
-- >>> boundGT 9 (VU.fromList [1,3,5,7,9 :: Int])
-- 5
boundGT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundGT x xs
  | VU.null xs = 0
  | otherwise =
      let (_, ok) = bisect (-1, VU.length xs) (\i -> xs VU.! i > x)
       in ok

-- | xより小さい値が最後に現れるインデックスを取得（境界外の場合は-1を返す）
--
-- >>> boundLT 5 (VU.fromList [1,3,5,7,9 :: Int])
-- 1
-- >>> boundLT 1 (VU.fromList [1,3,5,7,9 :: Int])
-- -1
boundLT :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundLT x xs
  | VU.null xs = -1
  | otherwise =
      let (ng, _) = bisect (-1, VU.length xs) (\i -> xs VU.! i >= x)
       in ng

-- | x以下の値が最後に現れるインデックスを取得（境界外の場合は-1を返す）
--
-- >>> boundLE 5 (VU.fromList [1,3,5,7,9 :: Int])
-- 2
-- >>> boundLE 0 (VU.fromList [1,3,5,7,9 :: Int])
-- -1
boundLE :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundLE x xs
  | VU.null xs = -1
  | otherwise =
      let (ng, _) = bisect (-1, VU.length xs) (\i -> xs VU.! i > x)
       in ng

module BSearch where

{-- 二分探索の核（コンテナ非依存） --}

-- 添字（や答えの値）の区間に対して境界を引くだけの関数群。
-- Array 版・Vector 版の二分探索は、この境界引きの上に組み立てる。

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

-- | モナド版二分探索。
-- 判定に可変配列の読み出しが要るとき（LIS の tails など）に使う。
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

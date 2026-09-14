{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BSearchVector (module BSearch, module BSearchVector) where

import BSearch
import Data.Vector.Unboxed qualified as VU

{-- 二分探索 Vector.Unboxed版 --}

-- 命名: 末尾の V は Vector.Unboxed 版であることを表す（IArray 版は A）。
-- バンドラが全モジュールを 1 ファイルに展開するため、Array 版と Vector 版で名前を分けている。

-- | x以上の値が最初に現れるインデックスを取得
--
-- >>> lookupGEIdxV 4 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 2
-- >>> lookupGEIdxV 10 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGEIdxV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupGEIdxV x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundGEV x xs
       in if i >= VU.length xs
            then Nothing
            else Just i

-- | xより大きい値が最初に現れるインデックスを取得
--
-- >>> lookupGTIdxV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 3
-- >>> lookupGTIdxV 9 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGTIdxV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupGTIdxV x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundGTV x xs
       in if i >= VU.length xs
            then Nothing
            else Just i

-- | xより小さい値が最後に現れるインデックスを取得
--
-- >>> lookupLTIdxV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 1
-- >>> lookupLTIdxV 1 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLTIdxV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupLTIdxV x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundLTV x xs
       in if i < 0
            then Nothing
            else Just i

-- | x以下の値が最後に現れるインデックスを取得
--
-- >>> lookupLEIdxV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 2
-- >>> lookupLEIdxV 0 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLEIdxV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe Int
lookupLEIdxV x xs
  | VU.null xs = Nothing
  | otherwise =
      let i = boundLEV x xs
       in if i < 0
            then Nothing
            else Just i

-- | x以上の値が最初に現れる値を取得
--
-- >>> lookupGEV 4 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 5
-- >>> lookupGEV 10 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGEV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupGEV x xs = do
  i <- lookupGEIdxV x xs
  return (xs VU.! i)

-- | xより大きい値が最初に現れる値を取得
--
-- >>> lookupGTV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 7
-- >>> lookupGTV 9 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupGTV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupGTV x xs = do
  i <- lookupGTIdxV x xs
  return (xs VU.! i)

-- | xより小さい値が最後に現れる値を取得
--
-- >>> lookupLTV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 3
-- >>> lookupLTV 1 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLTV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupLTV x xs = do
  i <- lookupLTIdxV x xs
  return (xs VU.! i)

-- | x以下の値が最後に現れる値を取得
--
-- >>> lookupLEV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- Just 5
-- >>> lookupLEV 0 (VU.fromList [1,3,5,7,9 :: Int])
-- Nothing
lookupLEV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Maybe e
lookupLEV x xs = do
  i <- lookupLEIdxV x xs
  return (xs VU.! i)

-- | x以上の値が最初に現れるインデックスを取得（境界外の場合は配列長を返す）
--
-- >>> boundGEV 4 (VU.fromList [1,3,5,7,9 :: Int])
-- 2
-- >>> boundGEV 10 (VU.fromList [1,3,5,7,9 :: Int])
-- 5
boundGEV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundGEV x xs
  | VU.null xs = 0
  | otherwise =
      let (_, ok) = bisect (-1, VU.length xs) (\i -> xs VU.! i >= x)
       in ok

-- | xより大きい値が最初に現れるインデックスを取得（境界外の場合は配列長を返す）
--
-- >>> boundGTV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- 3
-- >>> boundGTV 9 (VU.fromList [1,3,5,7,9 :: Int])
-- 5
boundGTV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundGTV x xs
  | VU.null xs = 0
  | otherwise =
      let (_, ok) = bisect (-1, VU.length xs) (\i -> xs VU.! i > x)
       in ok

-- | xより小さい値が最後に現れるインデックスを取得（境界外の場合は-1を返す）
--
-- >>> boundLTV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- 1
-- >>> boundLTV 1 (VU.fromList [1,3,5,7,9 :: Int])
-- -1
boundLTV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundLTV x xs
  | VU.null xs = -1
  | otherwise =
      let (ng, _) = bisect (-1, VU.length xs) (\i -> xs VU.! i >= x)
       in ng

-- | x以下の値が最後に現れるインデックスを取得（境界外の場合は-1を返す）
--
-- >>> boundLEV 5 (VU.fromList [1,3,5,7,9 :: Int])
-- 2
-- >>> boundLEV 0 (VU.fromList [1,3,5,7,9 :: Int])
-- -1
boundLEV :: (VU.Unbox e, Ord e) => e -> VU.Vector e -> Int
boundLEV x xs
  | VU.null xs = -1
  | otherwise =
      let (ng, _) = bisect (-1, VU.length xs) (\i -> xs VU.! i > x)
       in ng

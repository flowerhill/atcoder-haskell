-- | 平面幾何（距離・座標変換・回転）まわりのユーティリティ。
module Geometry where

-- | マンハッタン距離 |x1-x2| + |y1-y2| を求める
--
-- >>> manhattanDistance (0, 0) (3, 4 :: Int)
-- 7
manhattanDistance :: (Num a) => (a, a) -> (a, a) -> a
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | ユークリッド距離を求める
--
-- >>> euclidDistance (0.0, 0.0) (3.0, 4.0 :: Double)
-- 5.0
euclidDistance :: (Floating a) => (a, a) -> (a, a) -> a
euclidDistance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- | 整数座標間のユークリッド距離（Float）
--
-- >>> euclidDistFromInt (0, 0) (3, 4)
-- 5.0
euclidDistFromInt :: (Int, Int) -> (Int, Int) -> Float
euclidDistFromInt (x1, y1) (x2, y2) = sqrt $ (fromIntegral x1 - fromIntegral x2) ^ 2 + (fromIntegral y1 - fromIntegral y2) ^ 2

-- | 整数座標間のユークリッド距離の2乗（sqrt なし）
--
-- >>> euclidDistFromInt2 (0, 0) (3, 4)
-- 25
euclidDistFromInt2 :: (Int, Int) -> (Int, Int) -> Int
euclidDistFromInt2 (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- | 2次元ベクトルの外積（z成分）。0 なら2ベクトルは平行
--
-- >>> cross (1, 0) (0, 1 :: Int)
-- 1
-- >>> cross (2, 4) (1, 2 :: Int)
-- 0
-- >>> cross (-1, 1) (2, 2 :: Int)
-- -4
cross :: (Num a) => (a, a) -> (a, a) -> a
cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

-- | 2次元ベクトルの内積（スカラ積）。0 なら2ベクトルは直交
--
-- >>> dot (1, 0) (0, 1 :: Int)
-- 0
-- >>> dot (2, 3) (4, 5 :: Int)
-- 23
-- >>> dot (-1, 1) (2, 2 :: Int)
-- 0
dot :: (Num a) => (a, a) -> (a, a) -> a
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- | 極座標 (r, θ) を直交座標 (x, y) に変換する
--   x = r cos θ, y = r sin θ
--
-- >>> polarToCartesian 5 0
-- (5.0,0.0)
-- >>> polarToCartesian 0 0
-- (0.0,0.0)
-- >>> fst (polarToCartesian 1 pi)
-- -1.0
polarToCartesian :: Double -> Double -> (Double, Double)
polarToCartesian r theta = (r * cos theta, r * sin theta)

-- | 直交座標 (x, y) を極座標 (r, θ) に変換する
--   r = √(x² + y²),  θ = atan2 y x ∈ (-π, π]
--
-- >>> cartesianToPolar 3 4
-- (5.0,0.9272952180016122)
-- >>> cartesianToPolar 1 0
-- (1.0,0.0)
-- >>> cartesianToPolar 0 0
-- (0.0,0.0)
cartesianToPolar :: Double -> Double -> (Double, Double)
cartesianToPolar x y = (sqrt (x * x + y * y), atan2 y x)

-- | 45°回転 (x, y) -> (x + y, x - y)。
-- 回転後の座標では「各軸の差の絶対値の max」が元のマンハッタン距離に一致する。
--
-- >>> rot45 (3, 1)
-- (4,2)
-- >>> let (u1, v1) = rot45 (3, 1); (u2, v2) = rot45 (0, 0)
-- >>> max (abs (u1 - u2)) (abs (v1 - v2))
-- 4
rot45 :: (Int, Int) -> (Int, Int)
rot45 (x, y) = (x + y, x - y)

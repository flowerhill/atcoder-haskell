-- 等差数列の和を計算する関数
-- a: 初項
-- d: 公差
-- n: 項数

arithmeticSum :: Integral a => a -> a -> a -> a
arithmeticSum a d n = n * (2 * a + (n - 1) * d) `div` 2
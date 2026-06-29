{-# LANGUAGE TypeApplications #-}

-- | mod 10^9+7 のモジュラ演算と IntMod 型。素の整数演算は "Math" を参照。
module ModInt where

import qualified Data.List as L
import Math (exEuclid, sumOfArith)

modulus :: Int
modulus = 1000000007

-- | mod 10^9+7 の加算
--
-- >>> addMod 1000000006 1
-- 0
-- >>> addMod 3 4
-- 7
addMod :: Int -> Int -> Int
addMod x y = (x + y) `mod` modulus
{-# INLINE addMod #-}

-- | mod 10^9+7 の減算
--
-- >>> subMod 5 3
-- 2
subMod :: Int -> Int -> Int
subMod x y = (x - y) `mod` modulus
{-# INLINE subMod #-}

-- | mod 10^9+7 の乗算
--
-- >>> mulMod 100000 100000
-- 999999937
mulMod :: Int -> Int -> Int
mulMod x y = (x * y) `mod` modulus
{-# INLINE mulMod #-}

-- | mod 10^9+7 のリスト総和
--
-- >>> sumMod [1, 2, 3]
-- 6
sumMod :: [Int] -> Int
sumMod = L.foldl' addMod 0
{-# INLINE sumMod #-}

-- | mod 10^9+7 の除算（モジュラ逆数を利用）
--
-- >>> divMod2 6 2
-- 3
divMod2 :: Int -> Int -> Int
divMod2 x y = x `mulMod` powMod y (modulus - 2)
{-# INLINE divMod2 #-}

-- | mod 10^9+7 のリスト総積
--
-- >>> productMod [2, 3, 4]
-- 24
productMod :: [Int] -> Int
productMod = L.foldl' mulMod 1
{-# INLINE productMod #-}

-- | 繰り返し二乗法による mod 10^9+7 のべき乗
--
-- >>> powMod 2 10
-- 1024
-- >>> powMod 2 0
-- 1
powMod :: Int -> Int -> Int
powMod base exp
  | exp < 0 = error "powMod: negative exponent"
  | exp == 0 = 1
  | otherwise = powMod' (base `mod` modulus) exp 1
  where
    powMod' _ 0 acc = acc
    powMod' b e acc
      | e `mod` 2 == 1 = powMod' (mulMod b b) (e `div` 2) (mulMod acc b)
      | otherwise = powMod' (mulMod b b) (e `div` 2) acc
{-# INLINE powMod #-}

newtype IntMod = IntMod Int deriving (Eq, Show, Read)

instance Num IntMod where
  IntMod x + IntMod y = IntMod (addMod x y)
  IntMod x - IntMod y = IntMod (subMod x y)
  IntMod x * IntMod y = IntMod (mulMod x y)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral @Int modulus))
  abs = undefined
  signum = undefined

-- | Int から IntMod への変換
--
-- >>> toIntMod 5
-- IntMod 5
-- >>> toIntMod 1000000010
-- IntMod 3
toIntMod :: Int -> IntMod
toIntMod a = IntMod (a `mod` modulus)

-- | 等比数列の和 (mod 10^9+7): 初項 a, 公比 r, 項数 n
-- a * (r^n - 1) / (r - 1) を mod 10^9+7 で計算
-- r == 1 の場合は a * n を返す
--
-- >>> sumOfGeoMod 1 2 4
-- 15
-- >>> sumOfGeoMod 3 3 3
-- 39
-- >>> sumOfGeoMod 1 1 5
-- 5
sumOfGeoMod :: Int -> Int -> Int -> Int
sumOfGeoMod a r n
  | r == 1 = a `mulMod` (n `mod` modulus)
  | otherwise = a `mulMod` ((powMod r n `subMod` 1) `divMod2` ((r - 1) `mod` modulus))

-- | 等差数列の和 (mod 10^9+7): a から b までの公差1の整数の総和
--
-- >>> sumOfArithMod 1 10
-- 55
-- >>> sumOfArithMod 1 100000
-- 49965
sumOfArithMod :: Integer -> Integer -> Int
sumOfArithMod a b = fromInteger $ sumOfArith a b `mod` fromIntegral modulus

-- | mod 10^9+7 のモジュラ逆数（拡張ユークリッド利用）
--
-- >>> invMod 1
-- 1
-- >>> invMod 3
-- 333333336
invMod :: Int -> Int
invMod a = case exEuclid a modulus of
  (1, s, _) -> s `mod` modulus
  (-1, s, _) -> (-s) `mod` modulus
  _anyOfFailure -> error $ show a ++ " has no inverse modulo" ++ show @Int modulus

-- | IntMod のモジュラ逆数
--
-- >>> invIntMod (IntMod 1)
-- IntMod 1
-- >>> invIntMod (IntMod 2)
-- IntMod 500000004
invIntMod :: IntMod -> IntMod
invIntMod (IntMod a) = IntMod (invMod a)

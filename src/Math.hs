{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 数論・整数演算まわりのユーティリティ（mod 付き演算は "ModInt" を参照）。
module Math where

import Control.Monad (forM_, when)
import Control.Monad.ST.Strict (ST)
import qualified Data.Array.IArray as IA
import Data.Array.ST (STUArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray)
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Foreign (FiniteBits (countLeadingZeros, finiteBitSize))

-- | 三分探索: 凸関数 f の最小値付近の区間 (l, r) を返す（r - l <= 2）
--
-- >>> trisect (0, 10) (\x -> (x - 5) ^ 2)
-- (4,6)
-- >>> trisect (0, 6) (\x -> abs (x - 3))
-- (2,4)
trisect :: (Int, Int) -> (Int -> Int) -> (Int, Int)
trisect (l, r) f
  | r - l <= 2 = (l, r)
  | f m1 > f m2 = trisect (m1, r) f
  | otherwise = trisect (l, m2) f
  where
    m1 = (l * 2 + r) `div` 3
    m2 = (l + r * 2) `div` 3

-- | 素数判定（2以上を対象。1はTrueを返す点に注意）
--
-- >>> isPrime 7
-- True
-- >>> isPrime 4
-- False
-- >>> isPrime 2
-- True
isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) lst
  where
    xSqrt = floor (sqrt $ fromIntegral x :: Double) :: Int
    lst = [2 .. xSqrt]

-- | エラトステネスの篩: n以下の素数集合を返す。O(N log log N)
--
-- IntSet を p ごとに difference で削るとバランス木を何度も作り直すことになるので、
-- 篩は Unboxed の Bool ベクタ上で潰し、最後に一度だけ IntSet に詰め替える。
-- 合成数を弾くのは p が素数のときだけ、開始点も p*p でよい。
--
-- >>> IS.toList (sieve 20)
-- [2,3,5,7,11,13,17,19]
-- >>> IS.toList (sieve 1)
-- []
-- >>> IS.size (sieve 100)
-- 25
sieve :: Int -> IS.IntSet
sieve n
  | n < 2 = IS.empty
  | otherwise = IS.fromDistinctAscList [i | i <- [2 .. n], VU.unsafeIndex isPrime' i]
  where
    isPrime' :: VU.Vector Bool
    isPrime' = VU.create $ do
      v <- VUM.replicate (n + 1) True
      VUM.write v 0 False
      VUM.write v 1 False
      forM_ (takeWhile (\p -> p * p <= n) [2 ..]) $ \p -> do
        prime <- VUM.read v p
        when prime $
          forM_ [p * p, p * p + p .. n] $ \q ->
            VUM.write v q False
      return v

-- | Sieve: count distinct prime factors for each number up to n
-- O(N log log N)
--
-- >>> (IA.!) (countPrimeFactors 10) 6
-- 2
-- >>> (IA.!) (countPrimeFactors 10) 7
-- 1
countPrimeFactors :: Int -> UArray Int Int
countPrimeFactors n = runSTUArray $ do
  arr <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  forM_ [2 .. n] $ \i -> do
    v <- readArray arr i
    when (v == 0) $ do
      forM_ [i, i + i .. n] $ \j -> do
        cur <- readArray arr j
        writeArray arr j $! (cur + 1)
  return arr

-- | Sieve: smallest prime factor for each number up to n
-- O(N log log N)
--
-- >>> (IA.!) (smallestPrimeFactor 10) 6
-- 2
-- >>> (IA.!) (smallestPrimeFactor 10) 9
-- 3
smallestPrimeFactor :: Int -> UArray Int Int
smallestPrimeFactor n = runSTUArray $ do
  arr <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  forM_ [2 .. n] $ \i -> do
    v <- readArray arr i
    when (v == 0) $ do
      forM_ [i, i + i .. n] $ \j -> do
        cur <- readArray arr j
        when (cur == 0) $ writeArray arr j i
  return arr

-- | Fast prime factorization using smallest prime factor table
-- O(log n) per query after building the table
--
-- >>> let spf = smallestPrimeFactor 20 in primeFactors spf 12
-- [3,2]
-- >>> let spf = smallestPrimeFactor 20 in primeFactors spf 1
-- []
primeFactors :: UArray Int Int -> Int -> [Int]
primeFactors spf = go []
  where
    go !acc 1 = acc
    go !acc x =
      let p = spf IA.! x
          x' = divAll x p
       in go (p : acc) x'
    divAll x p
      | x `mod` p == 0 = divAll (x `div` p) p
      | otherwise = x

-- | 完全立方数か判定する
--
-- >>> isCube 27
-- True
-- >>> isCube 8
-- True
-- >>> isCube 9
-- False
isCube :: Int -> Bool
isCube n = cubeRoot ^ 3 == n
  where
    cubeRoot = round (fromIntegral n ** (1 / 3 :: Double))

-- | 天井割り算
--
-- >>> ceilDiv 7.0 2.0
-- 4
-- >>> ceilDiv 6.0 2.0
-- 3
ceilDiv :: Double -> Double -> Int
ceilDiv a b = ceiling $ a / b

-- | Int版の天井除算 ceil(a / b)。b /= 0 を仮定する。
-- 任意符号で正しく動き、Double を経由しないので精度誤差がない。
-- O(1)
--
-- >>> ceilDivInt 7 3
-- 3
-- >>> ceilDivInt 6 3
-- 2
-- >>> ceilDivInt 0 5
-- 0
-- >>> ceilDivInt (-7) 3
-- -2
-- >>> ceilDivInt 7 (-3)
-- -2
ceilDivInt :: Int -> Int -> Int
ceilDivInt a b = negate ((negate a) `div` b)
{-# INLINE ceilDivInt #-}

-- | 拡張ユークリッド互除法: (gcd, s, t) を返す（a*s + b*t = gcd）
--
-- >>> exEuclid 3 5
-- (1,2,-1)
-- >>> exEuclid 6 4
-- (2,1,-1)
exEuclid :: Int -> Int -> (Int, Int, Int)
exEuclid = fn 1 0 0 1
  where
    fn s t s' t' a b
      | b == 0 = (a, s, t)
      | otherwise =
          let (q, r) = a `divMod` b
           in fn s' t' (s - q * s') (t - q * t') b r

-- | 等比数列の和: 初項 a, 公比 r, 項数 n の総和 a + ar + .. + ar^(n-1)。O(n)
--
-- 公式 a*(r^n - 1)/(r - 1) は r == 1 で 0 除算になり r^n も溢れやすいので、
-- ホーナー法 s <- s*r + a で 1 項ずつ畳み込む（途中の値は常に答え以下）。
-- mod を取るなら "ModInt" の sumOfGeoMod を使う。
--
-- >>> sumOfGeo 1 2 4
-- 15
-- >>> sumOfGeo 3 3 3
-- 39
-- >>> sumOfGeo 3 1 5
-- 15
-- >>> sumOfGeo 1 (-2) 4
-- -5
-- >>> sumOfGeo 7 2 0
-- 0
sumOfGeo :: (Integral a, Integral b) => a -> a -> b -> a
sumOfGeo a r n = L.foldl' (\s _ -> s * r + a) 0 [1 .. n]

-- | 等差数列の和: 初項 a, 公差 d, 項数 n の総和 n*a + d*n(n-1)/2
--
-- n(n-1) を作る前に偶数側を 2 で割るので、答えが型に収まる限り途中で溢れない。
-- 区間 [a, b] の総和（公差1）は "sumOfRange" を使う。
--
-- >>> sumOfArith 1 1 10
-- 55
-- >>> sumOfArith 2 3 4
-- 26
-- >>> sumOfArith 3 (-2) 4
-- 0
-- >>> sumOfArith 5 0 3
-- 15
-- >>> sumOfArith 7 2 0
-- 0
sumOfArith :: (Integral a, Integral b) => a -> a -> b -> a
sumOfArith a d n
  | n <= 0 = 0
  | otherwise = a * n' + d * steps
  where
    n' = fromIntegral n
    -- n(n-1)/2 = 0+1+..+(n-1)。n と n-1 の偶数側を先に 2 で割る
    steps
      | even n' = (n' `div` 2) * (n' - 1)
      | otherwise = n' * ((n' - 1) `div` 2)

-- | 等差数列（公差1）の和: 区間 [a, b] の整数の総和。空区間 (a > b) は 0
--
-- >>> sumOfRange 1 10
-- 55
-- >>> sumOfRange 3 5
-- 12
-- >>> sumOfRange 7 7
-- 7
-- >>> sumOfRange 5 3
-- 0
sumOfRange :: (Integral a) => a -> a -> a
sumOfRange a b = sumOfArith a 1 (b - a + 1)

-- | 整数の平方根を求める（ニュートン法、精度保証あり）
--
-- >>> iSqrt 9
-- 3
-- >>> iSqrt 10
-- 3
-- >>> iSqrt 0
-- 0
iSqrt :: Integer -> Integer
iSqrt n
  | n < 0 = error "invalid parameter: cannot use negative number."
  | n == 0 = 0
  | otherwise = go (n `div` 2)
  where
    go x
      | x * x <= n && (x + 1) * (x + 1) > n = x
      | x * x > n = go ((x + n `div` x) `div` 2)
      | otherwise = go (x + 1)

-- | 二項係数 nCr（組み合わせ数）
--
-- >>> nCr 5 2
-- 10
-- >>> nCr 5 0
-- 1
-- >>> nCr 3 5
-- 0
nCr :: Int -> Int -> Int
nCr n r
  | r > n = 0
  | r > n - r = nCr n (n - r)
  | otherwise = product [n - r + 1 .. n] `div` product [1 .. r]

-- | 順列数 nPr
--
-- >>> nPr 5 2
-- 20
-- >>> nPr 5 0
-- 1
-- >>> nPr 3 5
-- 0
nPr :: Int -> Int -> Int
nPr n r
  | r > n = 0
  | otherwise = product [n - r + 1 .. n]

-- | ⌈log₂ k⌉ を計算する。
--
-- >>> ceilLog2 1
-- 0
-- >>> ceilLog2 4
-- 2
-- >>> ceilLog2 5
-- 3
ceilLog2 :: Int -> Int
ceilLog2 k
  | k <= 1 = 0
  | otherwise = finiteBitSize k - countLeadingZeros (k - 1)

-- | 整数を2進数の Bool リストに変換する（LSB 先頭）
--
-- >>> toBinary 6
-- [False,True,True]
-- >>> toBinary 1
-- [True]
toBinary :: Int -> [Bool]
toBinary = L.unfoldr f
  where
    f 0 = Nothing
    f i = Just (q == 1, p)
      where
        (p, q) = i `divMod` 2

-- | n進数の各桁リストに変換する（最上位桁が先頭）
--
-- >>> toDigits 10 255
-- [2,5,5]
-- >>> toDigits 2 5
-- [1,0,1]
-- >>> toDigits 10 0
-- [0]
toDigits :: (Integral a) => a -> a -> [a]
toDigits _ 0 = [0]
toDigits n a = reverse $ L.unfoldr f a
  where
    f 0 = Nothing
    f x = Just (q, p)
      where
        (p, q) = divMod x n

-- | 桁リストから n 進数の整数に変換する
--
-- >>> fromDigits 10 [2,5,5 :: Int]
-- 255
-- >>> fromDigits 2 [1,0,1 :: Int]
-- 5
fromDigits :: (Foldable t, Num a) => a -> t a -> a
fromDigits n = L.foldl' (\acc b -> acc * n + b) 0

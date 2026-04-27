{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Util where

-- 追加
-- 追加

import BSearchVector (bisectM)
import Control.Monad (forM_, when)
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.Array.IArray as IA
import Data.Array.ST (STUArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray)
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace (traceShow)
import Foreign (FiniteBits (countLeadingZeros, finiteBitSize))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

{- Library -}

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

-- | エラトステネスの篩: n以下の素数集合を返す
--
-- >>> IS.toList (sieve 20)
-- [2,3,5,7,11,13,17,19]
sieve :: Int -> IS.IntSet
sieve n = go 2 (IS.fromList [2 .. n])
  where
    go p s
      | p * p > n = s
      | otherwise = go (p + 1) (IS.difference s (IS.fromList [p * p, p * p + p .. n]))

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

-- 回文

-- | 文字列が回文か判定する
--
-- >>> isPalindrome "aba"
-- True
-- >>> isPalindrome "abc"
-- False
-- >>> isPalindrome ""
-- True
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- | 長さkの部分文字列に回文が含まれるか判定する
--
-- >>> containsPalindrome 3 "abcba"
-- True
-- >>> containsPalindrome 3 "abcde"
-- False
containsPalindrome :: Int -> String -> Bool
containsPalindrome k s = L.or [s' == L.reverse s' | s' <- substringK k s]

-- | 整数が回文数か判定する
--
-- >>> isPalindromeInt 121
-- True
-- >>> isPalindromeInt 123
-- False
isPalindromeInt :: Int -> Bool
isPalindromeInt n =
  if even numDigits
    then leftHalf == reverse rightHalf
    else leftHalf == reverse (tail rightHalf)
  where
    str = show n
    numDigits = length str
    (leftHalf, rightHalf) = splitAt (numDigits `div` 2) str

{- memo回文平方数を求める処理 -}
{-   let lst = filter isPalindrome $ map (^ 3) [1 .. 1000000] -}

-- | クイックソート
--
-- >>> quicksort [3,1,4,1,5,9,2,6 :: Int]
-- [1,1,2,3,4,5,6,9]
-- >>> quicksort ([] :: [Int])
-- []
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

-- | リストのi番目とj番目の要素を入れ替える
--
-- >>> swapList 0 2 [1,2,3 :: Int]
-- [3,2,1]
-- >>> swapList 1 3 [1,2,3,4,5 :: Int]
-- [1,4,3,2,5]
swapList :: Int -> Int -> [a] -> [a]
swapList i j xs
  | i < 0 || i >= length xs || j < 0 || j >= length xs = xs
  | otherwise =
      let (ys, x : zs) = splitAt i xs
          (ws, y : vs) = splitAt (j - i - 1) zs
       in ys ++ [y] ++ ws ++ [x] ++ vs

-- | 天井割り算
--
-- >>> ceilDiv 7.0 2.0
-- 4
-- >>> ceilDiv 6.0 2.0
-- 3
ceilDiv :: Double -> Double -> Int
ceilDiv a b = ceiling $ a / b

-- IntMod

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
sumMod = L.foldl' addMod 0 -- foldl' → L.foldl'
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
productMod = L.foldl' mulMod 1 -- foldl' → L.foldl'
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

-- | 等比数列の和: a * (r^n - 1) / (r - 1)
--
-- >>> sumOfGeo 1 2 4
-- 15
-- >>> sumOfGeo 3 3 3
-- 39
sumOfGeo :: (Integral a, Integral b) => a -> a -> b -> a
sumOfGeo a r n = a * (r ^ n - 1) `div` (r - 1)

-- 等差数列の和
sumOfArith :: (Integral a, Integral b) => a -> a -> b -> a
sumOfArith = undefined

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

{-- 文字列操作 --}

-- | 部分文字列の出現回数を返す
--
-- >>> countSubstring "ab" "ababab"
-- 3
-- >>> countSubstring "xyz" "abcabc"
-- 0
countSubstring :: String -> String -> Int
countSubstring sub str = length $ filter (L.isPrefixOf sub) $ L.tails str -- isPrefixOf → L.isPrefixOf, tails → L.tails

-- | 部分文字列の出現位置インデックスリストを返す
--
-- >>> findSubstringIndices "ab" "ababab"
-- [0,2,4]
-- >>> findSubstringIndices "xy" "abcabc"
-- []
findSubstringIndices :: String -> String -> [Int]
findSubstringIndices sub str = [i | (i, s) <- zip [0 ..] (L.tails str), sub `L.isPrefixOf` s] -- tails → L.tails, isPrefixOf → L.isPrefixOf

-- | 位置 start から len 文字の部分文字列を返す
--
-- >>> substring 1 3 "abcdef"
-- "bcd"
-- >>> substring 0 2 "hello"
-- "he"
substring :: Int -> Int -> String -> String
substring start len str = take len (drop start str)

-- | 長さk の全部分文字列リストを返す
--
-- >>> substringK 2 "abcd"
-- ["ab","bc","cd"]
substringK :: Int -> String -> [String]
substringK k s = [substring i k s | i <- [0 .. length s - k]]

-- | 全長さの部分文字列を列挙する
--
-- >>> substrings "abc"
-- ["a","b","c","ab","bc","abc"]
substrings :: String -> [String]
substrings s = concat [substringK i s | i <- [1 .. length s]]

-- | 文字列のi番目の文字をt[i]に変更する
--
-- >>> changeChar "abc" "xyz" 1
-- "ayc"
-- >>> changeChar "abc" "xyz" 0
-- "xbc"
changeChar :: String -> String -> Int -> String
changeChar s t i = take i s ++ [t !! i] ++ drop (i + 1) s

-- | 重複なしの全順列を生成する
--
-- >>> distinctPermutations [1,2,3 :: Int]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- >>> distinctPermutations [1,1,2 :: Int]
-- [[1,1,2],[1,2,1],[2,1,1]]
distinctPermutations :: (Ord a) => [a] -> [[a]]
distinctPermutations vs = permute (length vs) (L.sort vs)
  where
    permute 0 _ = [[]]
    permute _ [] = []
    permute n xs = [x : ys | (x, xs') <- select xs, ys <- permute (n - 1) xs']

    select :: (Ord a) => [a] -> [(a, [a])]
    select [] = []
    select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs, y /= x]

-- | 連続する部分リストをすべて列挙する
--
-- >>> sublists [1,2,3 :: Int]
-- [[1],[1,2],[1,2,3],[2],[2,3],[3]]
sublists :: [a] -> [[a]]
sublists = concatMap (filter (not . null) . L.inits) . L.tails -- inits → L.inits, tails → L.tails

{-- リスト変更 --}

-- | インデックスを指定してリストの要素を更新する
--
-- >>> updateAt 1 99 [1,2,3 :: Int]
-- [1,99,3]
-- >>> updateAt 5 99 [1,2,3 :: Int]
-- [1,2,3]
updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ [] = [] -- 空リストの場合
updateAt n newVal (x : xs)
  | n < 0 = x : xs -- 負のインデックスの場合は元のリストを返す
  | n == 0 = newVal : xs -- 更新位置の場合
  | otherwise = x : updateAt (n - 1) newVal xs -- 再帰的に次の要素を処理

-- | 条件を満たす要素をすべて新しい値に更新する
--
-- >>> updateWhere (> 2) 0 [1,2,3,4 :: Int]
-- [1,2,0,0]
updateWhere :: (a -> Bool) -> a -> [a] -> [a]
updateWhere pred newVal [] = []
updateWhere pred newVal (x : xs)
  | pred x = newVal : updateWhere pred newVal xs -- 条件に合致する場合は更新
  | otherwise = x : updateWhere pred newVal xs -- それ以外の場合は元の値を保持

-- | 複数の (インデックス, 値) ペアでリストを一括更新する
--
-- >>> updateMultiple [(0, 10), (2, 30)] [1,2,3 :: Int]
-- [10,2,30]
updateMultiple :: [(Int, a)] -> [a] -> [a]
updateMultiple [] xs = xs
updateMultiple ((i, v) : updates) xs = updateMultiple updates (updateAt i v xs)

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

-- | 条件を満たす要素数を数える
--
-- >>> countBy (> 2) [1,2,3,4,5 :: Int]
-- 3
-- >>> countBy even [1,2,3,4 :: Int]
-- 2
countBy :: (Foldable t) => (e -> Bool) -> t e -> Int
countBy predicate = L.foldl' (\acc a -> if predicate a then acc + 1 else acc) 0 -- foldl' → L.foldl'

-- | ランレングス圧縮: 連続する同じ要素を (値, 個数) にまとめる
--
-- >>> runLengthEncode "aaabbc"
-- [('a',3),('b',2),('c',1)]
-- >>> runLengthEncode [1,1,2,3,3,3 :: Int]
-- [(1,2),(2,1),(3,3)]
runLengthEncode :: (Eq a) => [a] -> [(a, Int)]
runLengthEncode = map (\xs -> (head xs, length xs)) . L.group -- group → L.group

{-- digits --}

-- | 整数を2進数の Bool リストに変換する（LSB 先頭）
--
-- >>> toBinary 6
-- [False,True,True]
-- >>> toBinary 1
-- [True]
toBinary :: Int -> [Bool]
toBinary = L.unfoldr f -- unfoldr → L.unfoldr
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
toDigits n a = reverse $ L.unfoldr f a -- unfoldr → L.unfoldr
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
fromDigits n = L.foldl' (\acc b -> acc * n + b) 0 -- foldl' → L.foldl'

{-- math --}

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

{-- debug --}

-- | DEBUG環境変数が設定されていれば値を stderr に出力し () を返す
--
-- >>> dbg (42 :: Int)
-- ()
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

-- | DEBUG環境変数の値を取得する
getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

-- | 値を stderr に出力しながら同じ値を返す（デバッグ用）
--
-- >>> traceDbg (42 :: Int)
-- 42
-- 42
traceDbg :: (Show b) => b -> b
traceDbg itm = traceShow itm itm

-- | リストからちょうど j 個選んだときの合計値をすべて列挙する
--
-- >>> combSums [3,8] 1
-- [3,8]
-- >>> combSums [3,8] 2
-- [11]
-- >>> combSums [7,5,11] 0
-- [0]
combSums :: [Int] -> Int -> [Int]
combSums _ 0 = [0]
combSums [] _ = []
combSums (x : xs) j =
  map (+ x) (combSums xs (j - 1)) -- x を選ぶ
    ++ combSums xs j -- x を選ばない

{-- LIS / LDS --}

-- | 各位置 i で終わる最長増加部分列（狭義）の長さを返す
-- O(N log N)
--
-- >>> lisLengths (VU.fromList [3,1,4,1,5,9,2,6])
-- [1,1,2,1,3,4,2,4]
lisLengths :: VU.Vector Int -> VU.Vector Int
lisLengths xs = runST $ do
  let !n = VU.length xs
  tails <- VUM.replicate n (maxBound :: Int)
  result <- VUM.new n
  lenRef <- VUM.replicate 1 (0 :: Int)

  forM_ [0 .. n - 1] $ \i -> do
    let !x = xs VU.! i
    curLen <- VUM.read lenRef 0
    (_, !pos) <- bisectM (-1, curLen) $ \mid -> do
      v <- VUM.read tails mid
      return (v >= x)
    VUM.write tails pos x
    VUM.write result i (pos + 1)
    when (pos == curLen) $ VUM.write lenRef 0 (curLen + 1)

  VU.unsafeFreeze result

-- | 各位置 i から始まる最長減少部分列（狭義）の長さを返す
-- O(N log N)
--
-- >>> ldsLengths (VU.fromList [3,1,4,1,5,9,2,6])
-- [2,1,2,1,2,2,1,1]
ldsLengths :: VU.Vector Int -> VU.Vector Int
ldsLengths = VU.reverse . lisLengths . VU.reverse

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

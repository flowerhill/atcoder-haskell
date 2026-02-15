{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Util where

import qualified Data.IntSet as IS
import qualified Data.List as L
import Debug.Trace (traceShow)
-- 追加
import System.Environment (lookupEnv) -- 追加
import System.IO.Unsafe (unsafePerformIO)

{- Library -}
trisect :: (Int, Int) -> (Int -> Int) -> (Int, Int)
trisect (l, r) f
  | r - l <= 2 = (l, r)
  | f m1 > f m2 = trisect (m1, r) f
  | otherwise = trisect (l, m2) f
  where
    m1 = (l * 2 + r) `div` 3
    m2 = (l + r * 2) `div` 3

isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) lst
  where
    xSqrt = floor (sqrt $ fromIntegral x :: Double) :: Int
    lst = [2 .. xSqrt]

sieve :: Int -> IS.IntSet
sieve n = go 2 (IS.fromList [2 .. n])
  where
    go p s
      | p * p > n = s
      | otherwise = go (p + 1) (IS.difference s (IS.fromList [p * p, p * p + p .. n]))

isCube :: Int -> Bool
isCube n = cubeRoot ^ 3 == n
  where
    cubeRoot = round (fromIntegral n ** (1 / 3 :: Double))

-- 回文
-- String
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

containsPalindrome :: Int -> String -> Bool
containsPalindrome k s = L.or [s' == L.reverse s' | s' <- substringK k s]

-- Int
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

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

swapList :: Int -> Int -> [a] -> [a]
swapList i j xs
  | i < 0 || i >= length xs || j < 0 || j >= length xs = xs
  | otherwise =
      let (ys, x : zs) = splitAt i xs
          (ws, y : vs) = splitAt (j - i - 1) zs
       in ys ++ [y] ++ ws ++ [x] ++ vs

ceilDiv :: Double -> Double -> Int
ceilDiv a b = ceiling $ a / b

-- IntMod

modulus :: Int
modulus = 1000000007

addMod :: Int -> Int -> Int
addMod x y = (x + y) `mod` modulus
{-# INLINE addMod #-}

subMod :: Int -> Int -> Int
subMod x y = (x - y) `mod` modulus
{-# INLINE subMod #-}

mulMod :: Int -> Int -> Int
mulMod x y = (x * y) `mod` modulus
{-# INLINE mulMod #-}

sumMod :: [Int] -> Int
sumMod = L.foldl' addMod 0 -- foldl' → L.foldl'
{-# INLINE sumMod #-}

divMod2 :: Int -> Int -> Int
divMod2 x y = x `mulMod` powMod y (modulus - 2)
{-# INLINE divMod2 #-}

productMod :: [Int] -> Int
productMod = L.foldl' mulMod 1 -- foldl' → L.foldl'
{-# INLINE productMod #-}

-- 繰り返し二乗法を使っている
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

toIntMod :: Int -> IntMod
toIntMod a = IntMod (a `mod` modulus)

exEuclid :: Int -> Int -> (Int, Int, Int)
exEuclid = fn 1 0 0 1
  where
    fn s t s' t' a b
      | b == 0 = (a, s, t)
      | otherwise =
          let (q, r) = a `divMod` b
           in fn s' t' (s - q * s') (t - q * t') b r

-- 等比数列の和
sumOfGeo :: (Integral a, Integral b) => a -> a -> b -> a
sumOfGeo a r n = a * (r ^ n - 1) `div` (r - 1)

-- 等差数列の和
sumOfArith :: (Integral a, Integral b) => a -> a -> b -> a
sumOfArith = undefined

invMod :: Int -> Int
invMod a = case exEuclid a modulus of
  (1, s, _) -> s `mod` modulus
  (-1, s, _) -> (-s) `mod` modulus
  _anyOfFailure -> error $ show a ++ " has no inverse modulo" ++ show @Int modulus

invIntMod :: IntMod -> IntMod
invIntMod (IntMod a) = IntMod (invMod a)

{-- 文字列操作 --}

-- 出現回数
countSubstring :: String -> String -> Int
countSubstring sub str = length $ filter (L.isPrefixOf sub) $ L.tails str -- isPrefixOf → L.isPrefixOf, tails → L.tails

-- 出現場所
findSubstringIndices :: String -> String -> [Int]
findSubstringIndices sub str = [i | (i, s) <- zip [0 ..] (L.tails str), sub `L.isPrefixOf` s] -- tails → L.tails, isPrefixOf → L.isPrefixOf

substring :: Int -> Int -> String -> String
substring start len str = take len (drop start str)

substringK :: Int -> String -> [String]
substringK k s = [substring i k s | i <- [0 .. length s - k]]

substrings :: String -> [String]
substrings s = concat [substringK i s | i <- [1 .. length s]]

-- 文字列を1文字変更する関数
changeChar :: String -> String -> Int -> String
changeChar s t i = take i s ++ [t !! i] ++ drop (i + 1) s

-- その他
-- distinct_permutation の Haskell 実装
distinctPermutations :: (Ord a) => [a] -> [[a]]
distinctPermutations vs = permute (length vs) (L.sort vs)
  where
    permute 0 _ = [[]]
    permute _ [] = []
    permute n xs = [x : ys | (x, xs') <- select xs, ys <- permute (n - 1) xs']

    select :: (Ord a) => [a] -> [(a, [a])]
    select [] = []
    select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs, y /= x]

-- 部分リストを抽出
sublists :: [a] -> [[a]]
sublists = filter (not . null) . concatMap L.inits . L.tails -- inits → L.inits, tails → L.tails

{-- リスト変更 --}
-- インデックスと新しい値を指定して、リストの要素を更新する
updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ [] = [] -- 空リストの場合
updateAt n newVal (x : xs)
  | n < 0 = x : xs -- 負のインデックスの場合は元のリストを返す
  | n == 0 = newVal : xs -- 更新位置の場合
  | otherwise = x : updateAt (n - 1) newVal xs -- 再帰的に次の要素を処理

-- 条件に基づいて要素を更新する
updateWhere :: (a -> Bool) -> a -> [a] -> [a]
updateWhere pred newVal [] = []
updateWhere pred newVal (x : xs)
  | pred x = newVal : updateWhere pred newVal xs -- 条件に合致する場合は更新
  | otherwise = x : updateWhere pred newVal xs -- それ以外の場合は元の値を保持

-- 複数の要素を一度に更新する
updateMultiple :: [(Int, a)] -> [a] -> [a]
updateMultiple [] xs = xs
updateMultiple ((i, v) : updates) xs = updateMultiple updates (updateAt i v xs)

-- マンハッタン距離 ∣x1−x2∣ + ∣y1−y2∣ を求める
manhattanDistance :: (Num a) => (a, a) -> (a, a) -> a
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- ユークリッド距離を求める
euclidDistance :: (Floating a) => (a, a) -> (a, a) -> a
euclidDistance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

euclidDistFromInt :: (Int, Int) -> (Int, Int) -> Float
euclidDistFromInt (x1, y1) (x2, y2) = sqrt $ (fromIntegral x1 - fromIntegral x2) ^ 2 + (fromIntegral y1 - fromIntegral y2) ^ 2

-- sqrtせず2乗のまま求める
euclidDistFromInt2 :: (Int, Int) -> (Int, Int) -> Int
euclidDistFromInt2 (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- 整数の平方根を求める関数（ニュートン法）
-- 精度の問題でこちらを使った方が良い
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

countBy :: (Foldable t) => (e -> Bool) -> t e -> Int
countBy predicate = L.foldl' (\acc a -> if predicate a then acc + 1 else acc) 0 -- foldl' → L.foldl'

-- ランレングス圧縮
runLengthEncode :: (Eq a) => [a] -> [(a, Int)]
runLengthEncode = map (\xs -> (head xs, length xs)) . L.group -- group → L.group

{-- digits --}
toBinary :: Int -> [Bool]
toBinary = L.unfoldr f -- unfoldr → L.unfoldr
  where
    f 0 = Nothing
    f i = Just (q == 1, p)
      where
        (p, q) = i `divMod` 2

toDigits :: (Integral a) => a -> a -> [a]
toDigits _ 0 = [0]
toDigits n a = reverse $ L.unfoldr f a -- unfoldr → L.unfoldr
  where
    f 0 = Nothing
    f x = Just (q, p)
      where
        (p, q) = divMod x n

fromDigits :: (Foldable t, Num a) => a -> t a -> a
fromDigits n = L.foldl' (\acc b -> acc * n + b) 0 -- foldl' → L.foldl'

{-- math --}
nCr :: Int -> Int -> Int
nCr n r
  | r > n = 0
  | r > n - r = nCr n (n - r)
  | otherwise = product [n - r + 1 .. n] `div` product [1 .. r]

nPr :: Int -> Int -> Int
nPr n r
  | r > n = 0
  | otherwise = product [n - r + 1 .. n]

{-- debug --}
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

traceDbg :: (Show b) => b -> b
traceDbg itm = traceShow itm itm
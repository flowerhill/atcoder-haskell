{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.IArray
import qualified Data.ByteString.Char8 as BS
import Data.Char (GeneralCategory (Control), digitToInt, isSpace, readLitChar)
import qualified Data.Graph as G
import qualified Data.IntSet as IntSet
import Data.Ix
import qualified Data.List as L
import Data.Maybe (catMaybes, fromJust, maybeToList)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Debug.Trace (traceShow, traceShowId)

{- Library -}
{-- dijkstra --}
myDijkstra :: Ix v => (v -> [(v, Int)]) -> (v, v) -> [v] -> UArray v Int
myDijkstra nextStates (l, u_) v0s = runSTUArray $ do
  dist <- newArray (l, u_) maxBound

  forM_ v0s $ \v -> do
    writeArray dist v 0

  let queue = Heap.fromList $ map (Heap.Entry 0) v0s

  aux (Heap.uncons queue) dist
  return dist
  where
    aux Nothing _ = return ()
    aux (Just (Heap.Entry dv v, queue)) dist = do
      garbage <- (dv >) <$> readArray dist v

      if garbage
        then aux (Heap.uncons queue) dist -- skip
        else do
          queue' <-
            foldM
              ( \q (u, w) -> do
                  du <- readArray dist u

                  let dv' = dv + w

                  if dv' < du
                    then do
                      writeArray dist u dv'
                      return $ Heap.insert (Heap.Entry dv' u) q
                    else return q
              )
              queue
              (nextStates v)

          aux (Heap.uncons queue') dist

graph2 :: (Ix i, Foldable t) => (i, i) -> t (i, i) -> Array i [i]
graph2 b uvs = accumArray @Array (flip (:)) [] b $ concatMap (\(u, v) -> [(u, v), (v, u)]) uvs

{- input IO -}
readWords :: IO [String]
readWords = words <$> getLine

-- Int

readInt :: IO Int
readInt = readLn

getInts :: IO [Int]
getInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

getPairInt :: IO (Int, Int)
getPairInt = (\[a, b] -> (a, b)) <$> getInts

parseLineIntList :: BC.ByteString -> [Int]
parseLineIntList = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace)

readIntPairIntLineV :: Int -> IO (V.Vector (Int, Int))
readIntPairIntLineV n = V.fromList <$> replicateM n readPairInt

readIntPairIntLineVU :: Int -> IO (VU.Vector (Int, Int))
readIntPairIntLineVU n = VU.fromList <$> replicateM n readPairInt

-- Double

getDouble :: IO [Double]
getDouble = map read . words . BC.unpack <$> BC.getLine

-- others

fromListToTuple :: [Int] -> (Int, Int)
fromListToTuple [a, b] = (a, b)

readTuple :: String -> (String, Int)
readTuple input = (str, read num :: Int)
  where
    [str, num] = words input

withInTime :: Int -> Int -> Int -> Bool
withInTime start diff time
  | start <= end = time >= start && time < end
  | otherwise = time >= start || time < end

withInTimeDiff :: Int -> Int -> Int -> Bool
withInTimeDiff start diff time
  | start <= end = time >= start && time < end
  | otherwise = time >= start || time < end
  where
    end = (start + diff) `mod` 24

printYN :: Bool -> IO ()
printYN f = putStrLn $ bool "No" "Yes" f

printList :: Show a => [a] -> IO ()
printList lst = putStrLn $ unwords $ map show lst

readGrid :: Int -> IO (V.Vector (V.Vector Char))
readGrid n = V.fromList <$> replicateM n (V.fromList <$> getLine)

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

sieve :: Int -> IntSet.IntSet
sieve n = go 2 (IntSet.fromList [2 .. n])
  where
    go p s
      | p * p > n = s
      | otherwise = go (p + 1) (IntSet.difference s (IntSet.fromList [p * p, p * p + p .. n]))

printIntGrid :: (Show e, IArray a e, Ix v) => a (v, Int) e -> IO ()
printIntGrid grid = traverse_ (putStrLn . unwords . map show) $ chunksOf w (elems grid)
  where
    ((_, w1), (_, w2)) = bounds grid
    w = w2 + 1 - w1

move :: Char -> (Int, Int) -> (Int, Int)
move 'L' (i, j) = (i, j - 1)
move 'R' (i, j) = (i, j + 1)
move 'U' (i, j) = (i - 1, j)
move 'D' (i, j) = (i + 1, j)
move _ pos = pos

{-  data Query = Add String | Print | Del
main :: iO ()
main = do
    qs <- replicateM q do
    query <- words <$> getLine
    return $ case query of
      ["1", x] -> Add x
      ["2"] -> Print
      ["3"] -> Del -}

{- main :: IO ()
main = do
  [n, m] <- readInputInts
  abs <- fmap concat <$> replicateM m $ do
    [a, b] <- readInputInts
    return [(a, b), (b, a)] -}

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

quicksort :: Ord a => [a] -> [a]
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

-- リストのコンビネーション
combinationList :: [[a]] -> [[a]]
combinationList [] = [[]]
combinationList (xs : xss) = [x : ys | x <- xs, ys <- combinationList xss]

-- 数字のパターンを重複を許して出す
combinationsWithRepetition :: Int -> Int -> Int -> [[Int]]
combinationsWithRepetition l r = go
  where
    go 0 = [[]]
    go k = [x : xs | x <- [l .. r], xs <- go (k - 1), x <= head (x : xs) || null xs]

-- 指定した数だけ抽出して組み合わせる
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations n as@(_ : xs)
  | n == 0 = [[]]
  | n == 1 = map pure as
  | n == l = pure as
  | n > l = []
  | otherwise = run (l - 1) (n - 1) as $ combinations (n - 1) xs
  where
    l = length as

    run :: Int -> Int -> [a] -> [[a]] -> [[a]]
    run m k ys cs
      | m == k = map (ys ++) cs
      | otherwise = case take (m - k + 1) ys of
          (q : qs) -> do
            let dc = product [(m - k + 1) .. (m - 1)] `div` product [1 .. (k - 1)]
            map (q :) cs ++ run (m - 1) k qs (drop dc cs)
          [] -> error "Invalid Case"

dfs :: G.Graph -> IS.IntSet -> Int -> IS.IntSet
dfs g seen v
  | IS.member v seen = seen
  | otherwise = L.foldl' (dfs g) seen' nextVs
  where
    nextVs = g IA.! v
    seen' = IS.insert v seen

modifyArray :: (MArray a t m, Ix i) => a i t -> i -> (t -> t) -> m ()
modifyArray arr idx f = do
  v <- readArray arr idx
  writeArray arr idx $ f

ceilDiv :: Double -> Double -> Int
ceilDiv a b = ceiling $ a / b

-- IntMod

modulus :: Int
modulus = 998244353

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
sumMod = foldl' addMod 0
{-# INLINE sumMod #-}

productMod :: [Int] -> Int
productMod = foldl' mulMod 1
{-# INLINE productMod #-}

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
countSubstring sub str = length $ filter (isPrefixOf sub) $ tails str

-- 出現場所
findSubstringIndices :: String -> String -> [Int]
findSubstringIndices sub str = [i | (i, s) <- zip [0 ..] (tails str), sub `isPrefixOf` s]

substring :: Int -> Int -> String -> String
substring start len str = take len (drop start str)

substringK :: Int -> String -> [String]
substringK k s = [substring i k s | i <- [0 .. length s - k]]

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
sublists = filter (not . null) . concatMap inits . tails

-- 斜め方向に走査
diag :: (IArray UArray a) => UArray (Int, Int) a -> [[a]]
diag arr =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
      coords =
        [ [ (i, j) | i <- [minRow .. maxRow], j <- [minCol .. maxCol], i - j == sum
          ]
          | sum <- [minRow - maxCol .. maxRow - minCol]
        ]
   in [[arr ! c | c <- cc] | cc <- coords]

-- 逆斜め方向に走査
revDiag :: (IArray UArray a) => UArray (Int, Int) a -> [[a]]
revDiag arr =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
      coords =
        [ [ (i, j) | i <- [minRow .. maxRow], j <- [maxCol, maxCol - 1 .. minCol], i + j == sum
          ]
          | sum <- [minRow + minCol .. maxRow + maxCol]
        ]
   in [[arr ! c | c <- cc] | cc <- coords]

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
manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- ユークリッド距離を求める
euclidDistance :: Floating a => (a, a) -> (a, a) -> a
euclidDistance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

euclidDistFromInt :: (Int, Int) -> (Int, Int) -> Float
euclidDistFromInt (x1, y1) (x2, y2) = sqrt $ (fromIntegral x1 - fromIntegral x2) ^ 2 + (fromIntegral y1 - fromIntegral y2) ^ 2

-- sqrtせず2乗のまま求める
euclidDistFromInt2 :: (Int, Int) -> (Int, Int) -> Int
euclidDistFromInt2 (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

{-- debug --}
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

traceDbg :: Show b => b -> b
traceDbg itm = traceShow itm itm
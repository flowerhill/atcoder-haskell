{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Control.Applicative
import Control.Monad
import qualified Control.Monad
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

{- input IO -}
readWords :: IO [String]
readWords = words <$> getLine

readInt :: IO Int
readInt = readLn

getInts :: IO [Int]
getInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

readPairInt :: IO (Int, Int)
readPairInt = (\[a, b] -> (a, b)) . parseLineIntList <$> BC.getLine

parseLineIntList :: BC.ByteString -> [Int]
parseLineIntList = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace)

readIntPairIntLineV :: Int -> IO (V.Vector (Int, Int))
readIntPairIntLineV n = V.fromList <$> replicateM n readPairInt

readIntPairIntLineVU :: Int -> IO (VU.Vector (Int, Int))
readIntPairIntLineVU n = VU.fromList <$> replicateM n readPairInt

readTuple :: String -> (String, Int)
readTuple input = (str, read num :: Int)
  where
    [str, num] = words input

withInTime :: Int -> Int -> Bool
withInTime start time
  | start <= end = time >= start && time < end
  | otherwise = time >= start || time < end
  where
    end = (start + 9) `mod` 24

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

-- Array binarySearch
binarySearch :: (Ord t2, IArray a t2, Ix t1, Integral t1) => a t1 t2 -> t2 -> t1 -> t1 -> t1
binarySearch arr val low high
  | high < low = low
  | otherwise =
      let mid = low + (high - low) `div` 2
       in if arr ! mid <= val
            then binarySearch arr val (mid + 1) high
            else binarySearch arr val low (mid - 1)

-- binarySearchIdx
binarySearchIdx :: (Num a, Ord a) => V.Vector a -> a -> Int -> Int -> Maybe Int
binarySearchIdx arr val low high
  | high < low = Just mid
  | otherwise = case midVal of
      Nothing -> Nothing
      Just v
        | v == val -> Just mid
        | v < val -> binarySearchIdx arr val (mid + 1) high
        | otherwise -> binarySearchIdx arr val low (mid - 1)
  where
    mid = low + (high - low) `div` 2
    midVal = arr V.!? mid

-- binarySearchVal
binarySearchVal :: (Num a, Ord a) => V.Vector a -> a -> Int -> Int -> Maybe a
binarySearchVal arr val low high
  | high < low = midVal
  | otherwise = case midVal of
      Nothing -> Nothing
      Just v
        | v == val -> Just val
        | v < val -> binarySearchVal arr val (mid + 1) high
        | otherwise -> binarySearchVal arr val low (mid - 1)
  where
    mid = low + (high - low) `div` 2
    midVal = arr V.!? mid

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

bfs :: Int -> Graph -> Seq.Seq (Int, Int) -> M.Map Int Int -> M.Map Int Int
bfs n g queue visited
  | null queue = visited
bfs n g ((q, l) Seq.:<| queue) visited
  | M.member q visited = bfs n g queue visited
  | otherwise =
      let visited' = M.insert q l visited
          next = g A.! q
          queue' = queue Seq.>< Seq.fromList (L.map (,succ l) next)
       in bfs n g queue' visited'

dfs :: Graph -> IS.IntSet -> Int -> IS.IntSet
dfs g seen v
  | IS.member v seen = seen
  | otherwise = L.foldl' (dfs g) seen' next_vs
  where
    next_vs = g IA.! v
    seen' = IS.insert v seen

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

isPalindrome :: Int -> Bool
isPalindrome n =
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

swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray as i j = do
  !a <- readArray as i
  !b <- readArray as j
  writeArray as j a
  writeArray as i b

safeGetElement :: [a] -> Int -> Maybe a
safeGetElement [] _ = Nothing
safeGetElement (x : xs) 0 = Just x
safeGetElement (x : xs) n
  | n < 0 = Nothing
  | otherwise = safeGetElement xs (n - 1)

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

-- grid

printMatrix :: UArray (Int, Int) Int -> IO ()
printMatrix arr = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
  let rows = [[arr ! (row, col) | col <- [minCol .. maxCol - 1]] | row <- [minRow .. maxRow - 1]]
  putStr $ unlines [unwords (map show row) | row <- rows]

twoDimensionalSum :: UArray (Int, Int) Int -> UArray (Int, Int) Int
twoDimensionalSum arr =
  listArray bounds_ $
    concat $
      scanl1 (zipWith (+)) $
        map (scanl1 (+)) lists
  where
    bounds_ = bounds arr
    lists = LS.chunksOf ((snd . snd) bounds_) $ elems arr

findIndexFrom :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndexFrom i f s =
  case findIndex f (drop i s) of
    Just j -> Just (i + j)
    Nothing -> Nothing

elemIndexFrom :: (Eq a) => Int -> a -> [a] -> Maybe Int
elemIndexFrom i x = findIndexFrom i (== x)


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

-- array
swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray as i j = do
  !a <- readArray as i
  !b <- readArray as j
  writeArray as j a
  writeArray as i b

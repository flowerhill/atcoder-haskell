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

main :: IO ()
main = do
  getLine
  as <- readInputInts

  let asZipSorted = IM.fromList zip [1 ..] as

  print $ solve asZipSorted

solve :: V.Vector (Int, Int) -> [Int]
solve v = go (V.head v) []
  where
    go :: (Int, Int) -> [Int] -> [Int]
    go e lst
      | snd e == -1 = go (bSearch v (fst e)) [fst e]
      | snd e == (V.length v - 1) = lst ++ [fst e]
      | otherwise = go (bSearch v (fst e)) (lst ++ [fst e])

judge :: [BS.ByteString] -> String
judge g
  | null g = "Yes"
  | length g == 1 = if isPrefixOfA (head g) || isPrefixOfB (head g) || isPrefixOfC (head g) then "Yes" else "No"
  | length g == 2 = if isPrefixOfA (head g) && isPrefixOfB (g !! 1) || isPrefixOfB (head g) && isPrefixOfC (g !! 1) || isPrefixOfA (head g) && isPrefixOfC (g !! 1) then "Yes" else "No"
  | length g == 3 = if isPrefixOfA (head g) && isPrefixOfB (g !! 1) && isPrefixOfC (g !! 2) then "Yes" else "No"
  | otherwise = "No"
  where
    isPrefixOfA = BS.isPrefixOf (BS.pack "A")
    isPrefixOfB = BS.isPrefixOf (BS.pack "B")
    isPrefixOfC = BS.isPrefixOf (BS.pack "C")

{- Library -}
readInt :: IO Int
readInt = readLn

readInputInts :: IO [Int]
readInputInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

readPairInt :: IO (Int, Int)
readPairInt = (\[a, b] -> (a, b)) . parseLineIntList <$> BC.getLine

readIntPairIntLineV :: Int -> IO (V.Vector (Int, Int))
readIntPairIntLineV n = V.fromList <$> replicateM n readPairInt

readIntPairIntLineVU :: Int -> IO (VU.Vector (Int, Int))
readIntPairIntLineVU n = VU.fromList <$> replicateM n readPairInt

parseLineIntList :: BC.ByteString -> [Int]
parseLineIntList = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace)

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

binarySearch :: (Ord t2, IArray a t2, Ix t1, Integral t1) => a t1 t2 -> t2 -> t1 -> t1 -> t1
binarySearch arr val low high
  | high < low = low
  | otherwise =
      let mid = low + (high - low) `div` 2
       in if arr ! mid <= val
            then binarySearch arr val (mid + 1) high
            else binarySearch arr val low (mid - 1)

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
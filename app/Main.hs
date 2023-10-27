module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import qualified Data.Array.Unboxed as AU
import qualified Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Graph
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as IntSet
import qualified Data.IntSet as S
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Tree
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import Debug.Trace (traceShow)

data UF s = UF {parent :: VM.STVector s Int, rank :: VM.STVector s Int}

toID :: Int -> Int -> Int -> Int
toID w i j = i * w + j

initialize :: Int -> ST s (UF s)
initialize n = do
  p <- VM.generate n id
  r <- VM.replicate n 0
  return $ UF p r

findRoot :: UF s -> Int -> ST s Int
findRoot uf i = do
  p <- VM.read (parent uf) i
  if p == i
    then return i
    else do
      root <- findRoot uf p
      VM.write (parent uf) i root
      return root

union' :: UF s -> Int -> Int -> ST s ()
union' uf x y = do
  rootX <- findRoot uf x
  rootY <- findRoot uf y
  when (rootX /= rootY) $ do
    rankX <- VM.read (rank uf) rootX
    rankY <- VM.read (rank uf) rootY
    if rankX < rankY
      then do
        VM.write (parent uf) rootX rootY
      else
        if rankX > rankY
          then do
            VM.write (parent uf) rootY rootX
          else do
            VM.write (parent uf) rootY rootX
            VM.modify (rank uf) (+ 1) rootX

solve :: Int -> Int -> V.Vector (V.Vector Char) -> Int
solve h w grid = runST $ do
  uf <- initialize (h * w)
  let sensors = [(i, j) | i <- [0 .. (h - 1)], j <- [0 .. (w - 1)], grid V.! i V.! j == '#']
      dirs = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]

  forM_ sensors $ \(i, j) ->
    forM_ dirs $ \(di, dj) ->
      when (i + di >= 0 && i + di < h && j + dj >= 0 && j + dj < w && grid V.! (i + di) V.! (j + dj) == '#') $
        union' uf (toID w i j) (toID w (i + di) (j + dj))

  roots <- mapM (findRoot uf) [toID w i j | i <- [0 .. h - 1], j <- [0 .. w - 1], grid V.! i V.! j == '#']

  let uniqueRoots = Set.fromList roots
  return $ Set.size uniqueRoots

main :: IO ()
main = do
  (h, w) <- readPairInt
  grid <- V.fromList <$> replicateM h (V.fromList <$> getLine)

  print $ solve h w grid

{- Library -}
readInputInt :: IO Int
readInputInt = fst . fromJust . BS.readInt <$> BS.getLine

readInputIntList :: IO [Int]
readInputIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

readPairInt :: IO (Int, Int)
readPairInt = (\[a, b] -> (a, b)) . parseLineIntList <$> BS.getLine

readIntPairIntLineVector :: Int -> IO (VU.Vector (Int, Int))
readIntPairIntLineVector n = VU.fromList <$> replicateM n readPairInt

parseLineIntList :: BS.ByteString -> [Int]
parseLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

withInTime :: Int -> Int -> Bool
withInTime start time
  | start <= end = time >= start && time < end
  | otherwise = time >= start || time < end
  where
    end = (start + 9) `mod` 24

readGrid :: Int -> IO (V.Vector (V.Vector Char))
readGrid n = V.fromList <$> replicateM n (V.fromList <$> getLine)
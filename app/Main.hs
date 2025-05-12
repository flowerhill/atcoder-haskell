{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.ST
import Control.Monad.State (MonadState (get), StateT (StateT, runStateT))
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.IO.Internals (IOArray (IOArray), IOUArray (IOUArray))
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Bifoldable (Bifoldable (bifold))
import Data.Bifunctor (bimap)
import Data.Bits (xor)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BC
import Data.Char (digitToInt, ord)
import qualified Data.Char as C
import Data.Foldable (for_)
import Data.Foldable.Extra (traverse_)
import Data.Function (fix, on)
import Data.Graph.Inductive (neighbors')
import qualified Data.HashSet as HS
import qualified Data.Heap as H
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IORef as MV
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Ix
import Data.List (elemIndices, find, findIndex, foldl', foldl1', group, groupBy, isInfixOf, mapAccumL, nub, partition, permutations, scanl', sort, sortBy, sortOn, subsequences, tails, transpose, unfoldr)
import Data.List.Extra (breakEnd, chunksOf, groupOn, lower, maximumOn, nubOrd, snoc, splitOn, sum', upper)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Mutable (MutableRef (..))
import Data.Ord (Down (Down), comparing)
import Data.Ratio ((%))
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), ViewL ((:<)), ViewR ((:>)), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MV
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  [h, w] <- getInts
  ss <- replicateM h getLine

  let grid = makeCharGrid ((1, 1), (h, w)) ss
      dist = bfs f (-1) (bounds grid) [(s, 0) | s <- findArrayIndices (== 'E') grid]
        where
          f v = [undefined| d <- mv, grid !? (v + d) == Just '.']

  return ()

mv@[left, right, up, down] = [(0, -1), (0, 1), (-1, 0), (1, 0)] :: [(Int, Int)]

resolve dist v
  | dist ! v == 0 = 'E'
  | dist ! v == -1 = '#'
  | otherwise ==
      head [c | (c, d) <- zip ['<', '>', '^', 'v'] mv]

{-- lib --}
printGrid :: (Applicative f, IArray a i, Ix v) => ([i] -> f b) -> a (v, Int) i -> f ()
printGrid f grid = traverse_ f $ chunksOf w (elems grid)
  where
    ((_, w1), (_, w2)) = bounds grid
    w = w2 + 1 - w1

printCharGrid :: (IArray a Char, Ix v) => a (v, Int) Char -> IO ()
printCharGrid = printGrid putStrLn

-- bfs
bfs :: (Ix v) => (v -> [v]) -> Int -> (v, v) -> [(v, Int)] -> UArray v Int
bfs nextStates initial b v0s = runSTUArray $ do
  dist <- newArray b initial

  for_ v0s $ \(v0, d0) -> do
    writeArray dist v0 d0

  aux (Seq.fromList [v0 | (v0, _) <- v0s]) dist
  return dist
  where
    aux Empty _ = return ()
    aux (v :<| queue) dist = do
      d <- readArray dist v
      us <- filterM (fmap (== initial) . readArray dist) (nextStates v)

      queue' <- foldForM queue us $ \q u -> do
        writeArray dist u (d + 1)
        return $ q |> u

      aux queue' dist

-- grid
makeCharGrid :: (IArray UArray e, Ix i, Foldable t) => (i, i) -> t [e] -> UArray i e
makeCharGrid (l, h) lst = listArray @UArray (l, h) $ concat lst

-- inputs
getInts :: IO [Int]
getInts = unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

-- outputs
printYn :: Bool -> IO ()
printYn f = putStrLn $ bool "No" "Yes" f

printList :: (Show a) => [a] -> IO ()
printList lst = putStrLn $ unwords $ map show lst

-- list
fromListToTuple :: [Int] -> (Int, Int)
fromListToTuple [a, b] = (a, b)

countBy :: (Foldable t) => (e -> Bool) -> t e -> Int
countBy predicate = foldl' (\acc a -> if predicate a then acc + 1 else acc) 0

-- fold
foldFor' :: (Foldable t) => b -> t a -> (b -> a -> b) -> b
foldFor' initial xs f = foldl' f initial xs

foldForM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldForM initial xs m = foldM m initial xs

foldForM_ :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m ()
foldForM_ initial xs m = foldM_ m initial xs

-- Array用のfind
findArrayIndices :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> [i]
findArrayIndices predicate as = [i | (i, e) <- assocs as, predicate e]

-- Array用の(!?)
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i =
  let b = bounds arr
   in if inRange b i
        then Just (arr ! i)
        else Nothing

instance (Num a, Num b) => Num (a, b) where
  (+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
  (a1, b1) + (a2, b2) = (a1 + a2, b1 + b2)
  (*) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
  (a1, b1) * (a2, b2) = (a1 * a2, b1 * b2)
  abs :: (Num a, Num b) => (a, b) -> (a, b)
  abs (a1, b1) = (abs a1, abs b1)
  signum :: (Num a, Num b) => (a, b) -> (a, b)
  signum (a1, b1) = (signum a1, signum b1)
  fromInteger :: (Num a, Num b) => Integer -> (a, b)
  fromInteger i = (fromInteger i, fromInteger i)
  negate :: (Num a, Num b) => (a, b) -> (a, b)
  negate (a1, b1) = (negate a1, negate b1)

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
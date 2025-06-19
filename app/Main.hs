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
import Data.Array.ST ()
import Data.Array.ST.Safe (runSTUArray)
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
import Data.Graph.Inductive (deg', neighbors')
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

countFiles :: (Integral a) => [a] -> a -> a
countFiles as t = sum [t `div` a | a <- as]

main :: IO ()
main = do
  [n, k] <- getInts
  as <- getInts

  print $ snd $ bisect (-1, 10 ^ 9) (\t -> countFiles as t >= k)

{-- lib --}

-- | 左が false / 右が true で境界を引く
bisect :: (Integral a) => (a, a) -> (a -> Bool) -> (a, a)
bisect (ng, ok) f
  | abs (ok - ng) == 1 = (ng, ok)
  | f m = bisect (ng, m) f
  | otherwise = bisect (m, ok) f
  where
    m = (ok + ng) `div` 2

updateArray :: (MArray a t m, Ix i) => a i t -> i -> (t -> t) -> m ()
updateArray arr i f = do
  val <- readArray arr i
  writeArray arr i (f val)
{-# INLINE updateArray #-}

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
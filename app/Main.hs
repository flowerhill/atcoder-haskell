{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.ByteString.Char8 qualified as BC
import Data.Char (digitToInt, ord)
import Data.Char qualified as C
import Data.Foldable (for_)
import Data.Foldable.Extra (traverse_)
import Data.Function (fix, on)
import Data.Graph.Inductive (deg', neighbors')
import Data.HashSet qualified as HS
import Data.Heap qualified as H
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.IORef qualified as MV
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Ix
import Data.List
import Data.List.Extra
import Data.Map qualified as M
import Data.Maybe
import Data.Mutable
import Data.Ord
import Data.Ratio ((%))
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), ViewL ((:<)), ViewR ((:>)), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  return ()

{-- lib --}
-- inputs
getInts :: IO [Int]
getInts = unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

getInteger :: IO [Integer]
getInteger = unfoldr (BC.readInteger . BC.dropWhile C.isSpace) <$> BC.getLine

getTuple :: IO (Int, Int)
getTuple = fromListToTuple <$> getInts

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

-- 符号なし32bit整数の限界値
maxUnsignedInt32 = 4294967295

minUnsignedInt32 = -4294967295

-- 符号つき32bit整数の最大値
minSignedInt32 = -2147483648

-- 符号なし64bit整数の限界値
-- 64bit
maxUnsignedInt64 = 18446744073709551615

minUnsignedInt64 = -18446744073709551615

-- 符号つき64bit整数の最大値
maxSignedInt64 = 9223372036854775807

{-- debug --}
#ifdef DEBUG
dbg :: Show a => a -> ()
dbg !x = let !_ = traceShow x () in ()

fnDbg :: (Show a) => a -> a
fnDbg !x = traceShow x x
#else
dbg :: Show a => a -> ()
dbg _ = ()

fnDbg :: Show a => a -> a
fnDbg x = x
#endif
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
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.IO.Internals (IOArray (IOArray))
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Bifoldable (Bifoldable (bifold))
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BC
import Data.Char (digitToInt, ord)
import qualified Data.Char as C
import Data.Function (fix, on)
import Data.Graph.Inductive (neighbors')
import qualified Data.HashSet as HS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IORef as MV
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Ix
import Data.List (elemIndices, findIndex, foldl', group, groupBy, isInfixOf, mapAccumL, nub, partition, permutations, scanl', sort, sortBy, sortOn, subsequences, tails, transpose, unfoldr)
import Data.List.Extra (breakEnd, groupOn, lower, maximumOn, nubOrd, splitOn, sum', upper, (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Mutable (MutableRef (..))
import Data.Ord (Down (Down), comparing)
import Data.Ratio ((%))
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq ((:|>)), ViewL ((:<)), ViewR ((:>)))
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
  -- ここに解答を書く
  return ()

{-- lib --}
getInts :: IO [Int]
getInts = unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

printYN :: Bool -> IO ()
printYN f = putStrLn $ bool "No" "Yes" f

printList :: Show a => [a] -> IO ()
printList lst = putStrLn $ unwords $ map show lst

fromListToTuple :: [Int] -> (Int, Int)
fromListToTuple [a, b] = (a, b)

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

traceDbg :: Show b => b -> b
traceDbg itm = traceShow itm itm
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad (forM_, replicateM, when)
import Data.Array (Array)
import Data.Array.Base (UArray (UArray), getElems, readArray)
import Data.Array.IArray
import Data.Array.IO (IOUArray, MArray (newArray), readArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Char8 (readInt)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import Data.Ix
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Vector as V
import Debug.Trace
import GHC.IO
import System.Environment

main :: IO ()
main = do
  n <- readLn @Int
  lrs <- replicateM n getInts

  let res = solve lrs [] 0
  print res

  if snd res == 0
    then do
      putStrLn "Yes"
      putStrLn $ unwords . map show $ L.reverse $ fst res
    else putStrLn "No"

{-- lib --}
solve [] res sum = (res, sum)
solve ([l, r] : tail) res sum
  | l <= -sum && -sum <= r = solve tail (-sum : res) 0
  | -sum <= l = solve tail (l : res) (sum + l)
  | -sum >= r = solve tail (r : res) (sum + r)

getInts :: IO [Int]
getInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

minAbsValue :: (Num a, Ord a) => a -> a -> a
minAbsValue x y
  | abs x <= abs y = x
  | otherwise = y

{-- debug --}
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

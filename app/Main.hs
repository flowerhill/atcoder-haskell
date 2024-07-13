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
  [[x1, y1], [x2, y2], [x3, y3]] <- replicateM 3 getFloats

  let x = (x2 - x1) ** 2

  let len1 = (x2 - x1) ^ 2 + (y2 - y1) ^ 2
  let len2 = (x3 - x1) ^ 2 + (y3 - y1) ^ 2
  let len3 = (x3 - x2) ^ 2 + (y3 - y2) ^ 2

  let m = L.maximum [len1, len2, len3]
  let others = L.filter (/= m) [len1, len2, len3]

  putStrLn if m == L.sum others then "Yes" else "No"

{-- lib --}

getFloats :: IO [Float]
getFloats = map read . words . BC.unpack <$> BC.getLine

{-- debug --}
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

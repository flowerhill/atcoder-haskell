{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad (forM_, when)
import Data.Array (Array)
import Data.Array.Base (UArray (UArray), readArray)
import Data.Array.IArray
import Data.Array.IO (IOUArray, MArray (newArray), readArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Char8 (readInt)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import Data.Ix
import qualified Data.List as L
import qualified Data.Vector as V
import Debug.Trace
import GHC.IO
import System.Environment

main :: IO ()
main = do
  [n, s] <- getInts
  as <- getInts

  dp <- newArray ((0, 0), (n, s)) False :: IO (IOUArray (Int, Int) Bool)

  writeArray dp (0, 0) True

  forM_ [1 .. n] \i -> do
    forM_ [0 .. s] \j -> do
      v <- readArray dp (i - 1, j)
      let a = as L.!! (i - 1)
      when (j < a && v) $ writeArray dp (i, j) True
      when (j >= a) $ do
        v2 <- readArray dp (i - 1, j - a)
        when (v2 || v) $ writeArray dp (i, j) True

  result <- readArray dp (n, s)
  putStrLn $ if result then "Yes" else "No"

getInts :: IO [Int]
getInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

{-- debug --}
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}
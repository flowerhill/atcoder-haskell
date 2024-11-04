{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import Data.Ix
import Data.List (groupBy, sort, sortBy, sortOn, unfoldr)
import Data.List.Extra (breakEnd, lower, upper)
import Data.Ord (Down (Down), comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import GHC.List (foldl', scanl')
import System.Environment (lookupEnv)

main :: IO ()
main = do
  n <- readLn @Int
  s <- getLine
  q <- readLn @Int
  qs <- replicateM q do
    [t, x, c] <- words <$> getLine
    return (read @Int t, (read @Int x, head c))

  let f1 qq st = case qq of
        0 -> st
        2 -> lower st
        3 -> upper st
        _ -> error "Invalid last query"

      f2 st bounds query =
        let arr = listArray @UArray bounds st
         in elems $ accum (\_ x -> x) arr $ map snd $ filter (\(t, _) -> t == 1) query

      (pre, post) = breakEnd (\(t, _) -> t /= 1) qs
      lQ = if null pre then 0 else fst (last pre)
      s' = f1 lQ $ f2 s (1, n) pre

  putStrLn $ f2 s' (1, n) post

{-- lib --}
getInts :: IO [Int]
getInts = unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

{-- debug --}
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

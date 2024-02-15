{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (replicateM, when)
import Control.Monad.ST
import Control.Monad.State
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Debug.Trace (traceShow, traceShowId)

data Query = Add String | Print | Del

main :: IO ()
main = do
  !n <- readLn @Int
  !qs <- replicateM n $ do
    query <- words <$> getLine
    return $ case query of
      ["1", x] -> Add x
      ["2"] -> Print
      ["3"] -> Del

  solve qs []

solve :: [Query] -> [String] -> IO ()
solve [] _ = return ()
solve qs@((Add x) : qs') xs = solve qs' (x : xs)
solve qs@(Print : qs') stack@(x : _) = do
  putStrLn x
  solve qs' stack
solve (Del : qs') (x : xs') = solve qs' xs'

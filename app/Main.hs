{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Data.Array.IArray
import qualified Data.Array.IArray as IA
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import qualified Data.Heap as H
import qualified Data.List as L
import Data.Maybe
import Debug.Trace (traceShow)

data Query = Add Int | Print | Del

main :: IO ()
main = do
  q <- readLn @Int
  qs <- replicateM q do
    query <- words <$> getLine
    return $ case query of
      ["1", x] -> Add $ read x
      ["2"] -> Print
      ["3"] -> Del

  solve qs H.empty

solve :: [Query] -> H.Heap Int -> IO ()
solve [] _ = return ()
solve (Add s : qs') h = do
  solve qs' (H.insert s h)
solve (Print : qs') h = do
  print $ minimum h
  solve qs' h
solve (Del : qs') h = do
  solve qs' (H.deleteMin h)

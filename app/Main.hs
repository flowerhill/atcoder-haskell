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
import Data.Function (fix)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Ix
import Data.List (groupBy, sort, sortBy, sortOn, unfoldr)
import Data.List.Extra (breakEnd, lower, upper)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import GHC.List (foldl', scanl')
import System.Environment (lookupEnv)

main :: IO ()
main = do
  [n1, n2, m] <- getInts
  abs <- replicateM m getInts

  let n = n1 + n2
      g = buildG (1, n) abs
      d1 = maximum . elems $ bfs g (1, n) 1
      d2 = maximum . elems $ bfs g (1, n) n

  print $ d1 + d2 + 1

{-- lib --}
getInts :: IO [Int]
getInts = unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

type Graph = Array Int [Int]

type Bounds = (Int, Int)

-- グラフ構築
buildG :: Bounds -> [[Int]] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v), (v, u)]) edges

-- BFSを実行し、始点からの距離をUArrayで返す
bfs :: Graph -> Bounds -> Int -> UArray Int Int
bfs graph (l, h) start = runSTUArray $ do
  -- 距離配列を-1で初期化
  dist <- newArray (l, h) (-1) :: ST s (STUArray s Int Int)
  -- 探索キュー
  queue <- newSTRef (Seq.singleton start)
  -- 始点の距離を0に設定
  writeArray dist start 0

  -- メインのBFSループ
  fix $ \loop -> do
    q <- readSTRef queue
    if Seq.null q
      then return ()
      else do
        let (v Seq.:< rest) = Seq.viewl q
        writeSTRef queue rest

        -- 現在の頂点の距離を取得
        currentDist <- readArray dist v
        -- 隣接頂点を処理
        forM_ (graph ! v) $ \u -> do
          uDist <- readArray dist u
          when (uDist == -1) $ do
            writeArray dist u (currentDist + 1)
            modifySTRef' queue (Seq.>< Seq.singleton u)
        loop

  return dist

{-- debug --}
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

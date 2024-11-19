{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
import Data.List (foldl', group, groupBy, sort, sortBy, sortOn, unfoldr)
import Data.List.Extra (breakEnd, lower, splitOn, upper)
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
import System.Environment (lookupEnv)

main :: IO ()
main = do
  [h, w] <- getInts
  ss <- concat <$> replicateM h getLine

  let grid = listArray @UArray ((1, 1), (h, w)) ss
      dist = bfs (\v -> filter (judge grid v) $ move v) (-1) ((1, 1), (h, w)) [(1, 1)]

      res = dist ! (h, w)

  putStrLn $ if res == -1 then "No" else "Yes"

move :: (Num a, Num b) => (a, b) -> [(a, b)]
move pos = map (move pos) [(1, 0), (0, 1), (-1, 0), (0, -1)]
  where
    move (x, y) (x', y') = (x + x', y + y')

judge :: Ix i => UArray i Char -> i -> i -> Bool
judge grid x x'
  | (not . inRange (bounds grid)) x' = False
  | otherwise = case (grid ! x, grid ! x') of
      ('s', 'n') -> True
      ('n', 'u') -> True
      ('u', 'k') -> True
      ('k', 'e') -> True
      ('e', 's') -> True
      _ -> False

{-- lib --}
getInts :: IO [Int]
getInts = unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

-- 幅優先探索（グラフ用）
type Graph = Array Int [Int]

type Edge = (Int, Int)

type Bounds = (Int, Int)

-- BFSを実行し、始点からの距離をUArrayで返す
bfs :: (Ix i, Foldable t) => (i -> t i) -> Int -> (i, i) -> [i] -> UArray i Int
bfs nextStates init (l, h) starts = runSTUArray $ do
  -- 距離配列を初期化
  dist <- newArray (l, h) init
  -- 探索キュー
  queue <- newSTRef (Seq.fromList starts)
  -- 始点の距離を0に設定
  forM_ starts $ \start ->
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
        forM_ (nextStates v) $ \u -> do
          uDist <- readArray dist u
          when (uDist == init) $ do
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

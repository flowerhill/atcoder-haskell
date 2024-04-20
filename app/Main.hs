{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad (forM_, replicateM)
import Data.Array (listArray)
import Data.Array.Base (MArray (newArray), newListArray, writeArray)
import qualified Data.Array.IArray as IA
import Data.Array.IO (IOUArray, getElems)
import Data.Array.MArray (readArray)
import Data.Array.Unboxed (UArray)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import Data.Foldable (for_)
import Data.Ix (Ix)
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Traversable (for)

main :: IO ()
main = do
  n <- readLn @Int
  an <- readInputInts

  anArr <- newListArray @IOUArray (1, n) an
  pos <- newArray @IOUArray (1, n) (-1 :: Int)

  for_ (zip an [1 ..]) $ \(a, i) -> do
    writeArray pos a i

  res <- for [1 .. n] $ \a -> do
    -- aは今見ている位置と、aという値入れ替えたい要素の2つの意味がある。
    elemA <- readArray anArr a -- aの位置にいる要素を取得する
    posA <- readArray pos a -- aという値が存在する位置を取得する
    swapArray anArr a posA -- aの数列を更新する
    swapArray pos a elemA -- aの位置のメモを更新する
    return (a, posA)

  let res' = [(i, j) | (i, j) <- res, i /= j]

  print $ L.length res'
  forM_ res' (\(i, j) -> putStrLn $ show i ++ " " ++ show j)

swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray as i j = do
  !a <- readArray as i
  !b <- readArray as j
  writeArray as j a
  writeArray as i b

readInputInts :: IO [Int]
readInputInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

readPairInt :: IO (Int, Int)
readPairInt = (\[a, b] -> (a, b)) . parseLineIntList <$> BC.getLine

parseLineIntList :: BC.ByteString -> [Int]
parseLineIntList = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace)

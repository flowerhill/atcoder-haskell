{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.ByteString.Char8 (readInt)
import qualified Data.ByteString.Char8 as BC
import Data.Char (intToDigit)
import qualified Data.Char as C
import qualified Data.List as L
import Data.List.Split
import qualified Data.List.Split as L
import Debug.Trace

main :: IO ()
main = do
  [s, t] <- words <$> getLine

  let lst = L.concatMap (L.transpose . flip L.chunksOf s) [1 .. (L.length s - 1)]
  putStrLn $ if t `elem` lst then "Yes" else "No"

-- library --
getInts :: IO [Int]
getInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import qualified Data.HashSet as HS
import qualified Data.List as L

main :: IO ()
main = do
  _ <- readLn @Int
  as <- readInputInts
  _ <- readLn @Int
  bs <- readInputInts
  _ <- readLn @Int
  cs <- readInputInts
  _ <- readLn @Int
  xs <- readInputInts

  let sums = HS.fromList [x + y + z | x <- as, y <- bs, z <- cs]

  forM_ xs (\x -> putStrLn if HS.member x sums then "Yes" else "No")

readInputInts :: IO [Int]
readInputInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine
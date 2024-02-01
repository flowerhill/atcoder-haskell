{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe (catMaybes)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [n, _] <- L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine
  !xs <- L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

  print $ solve n xs

solve :: Int -> [Int] -> Int
solve n xs = minimum $ L.scanl1 (+) $ elems da
  where
    da =
      accumArray @UArray
        (+)
        0
        (1, n)
        [ p | (a, b) <- L.zipWith (\a b -> (if a <= b then (a, b) else (b, a))) xs (tail xs), let df = b - a, let db = n - df, p <- [(1, df), (a, db - df), (b, df - db)]
        ]

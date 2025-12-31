module Lib (solve) where

import Data.Array (Ix)
import Data.Array.Base

-- | 問題固有のロジック
-- この関数を書き換えて問題を解く
solve :: Int -> [Int] -> Int
solve n xs = sum xs

module Output where

import Data.Array.IArray (IArray, bounds, (!))
import Data.Bool (bool)
import Numeric (showFFloat)

-- 方針:
--   標準出力と出力用の整形を置く。
--   行ごとに putStrLn を繰り返さず、unlines でまとめて 1 回の putStr に流す。

{-- 標準出力 --}

-- | Bool を "Yes"/"No" として標準出力する
printYn :: Bool -> IO ()
printYn f = putStrLn $ bool "No" "Yes" f

-- | リストを空白区切りで1行標準出力する
printList :: (Show a) => [a] -> IO ()
printList lst = putStrLn $ unwords $ map show lst

-- | リストを1要素1行で標準出力する。
-- printList と違い縦に並べる。putStrLn の繰り返しを避けて 1 回の putStr にまとめる。
--
-- >>> printLines [1, 2, 3 :: Int]
-- 1
-- 2
-- 3
-- >>> printLines ([] :: [Int])
printLines :: (Show a) => [a] -> IO ()
printLines = putStr . unlines . map show

-- | グリッド（リストのリスト）を1行ずつ空白区切りで標準出力する。
-- 行ごとに putStrLn すると 1 行ごとにフラッシュ判定が走るので、
-- unlines でまとめて 1 回の putStr に流す。
--
-- >>> printGrid [[1, 2, 3], [4, 5, 6 :: Int]]
-- 1 2 3
-- 4 5 6
-- >>> printGrid ([] :: [[Int]])
printGrid :: (Show a) => [[a]] -> IO ()
printGrid = putStr . unlines . map (unwords . map show)

-- | タプルのリストを1行1組・空白区切りで標準出力する
--
-- >>> printPairs [(1, 2), (3, 4 :: Int)]
-- 1 2
-- 3 4
-- >>> printPairs ([] :: [(Int, Int)])
printPairs :: (Show a, Show b) => [(a, b)] -> IO ()
printPairs = putStr . unlines . map (\(a, b) -> show a ++ " " ++ show b)

-- | 2 次元配列を 1 行ずつ空白区切りで標準出力する（bounds の範囲をすべて出す）。
--
-- >>> import Data.Array.Unboxed (UArray, listArray)
-- >>> printMatrix (listArray ((1,1),(2,3)) [1..6] :: UArray (Int,Int) Int)
-- 1 2 3
-- 4 5 6
-- >>> printMatrix (listArray ((0,0),(0,0)) [7] :: UArray (Int,Int) Int)
-- 7
printMatrix :: (IArray a e, Show e) => a (Int, Int) e -> IO ()
printMatrix arr = printGrid [[arr ! (r, c) | c <- [c0 .. c1]] | r <- [r0 .. r1]]
  where
    ((r0, c0), (r1, c1)) = bounds arr

{-- 実数の整形 --}

-- | Double を小数点以下 d 桁の固定小数表記にする。
-- show だと 1.0e-9 のような指数表記になり実数ジャッジに通らないので、
-- 誤差許容付きの出力にはこちらを使う。
--
-- >>> showFixed 3 3.14159
-- "3.142"
-- >>> showFixed 6 (-1.5)
-- "-1.500000"
-- >>> showFixed 9 1e-9
-- "0.000000001"
showFixed :: Int -> Double -> String
showFixed d v = showFFloat (Just d) v ""

-- | Double のリストを小数点以下 d 桁で1行1個ずつ標準出力する
--
-- >>> printLinesFixed 2 [1, 2.346]
-- 1.00
-- 2.35
-- >>> printLinesFixed 2 []
printLinesFixed :: Int -> [Double] -> IO ()
printLinesFixed d = putStr . unlines . map (showFixed d)

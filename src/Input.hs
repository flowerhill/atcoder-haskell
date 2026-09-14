{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Input where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | 標準入力から Int を1つ読み込む
getInt :: IO Int
getInt = readLn @Int

-- | 標準入力から空白区切り Int リストを読み込む
getInts :: IO [Int]
getInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

-- | 標準入力の残り全体から、空白・改行区切りの Int リストを読み込む
-- 1 行 1 数値が大量に続く入力で使う
getIntsAll :: IO [Int]
getIntsAll = parseLineIntList <$> BC.getContents

-- | 標準入力から Integer を1つ読み込む
getInteger :: IO Integer
getInteger = readLn @Integer

-- | 標準入力から空白区切り Integer リストを読み込む
getIntegers :: IO [Integer]
getIntegers = L.unfoldr (BC.readInteger . BC.dropWhile C.isSpace) <$> BC.getLine

-- | 標準入力から空白区切り ByteString リストを読み込む
getWords :: IO [BC.ByteString]
getWords = BC.words <$> BC.getLine

-- | n行の整数グリッドを読み込む
getGrid :: Int -> IO [[Int]]
getGrid n = replicateM n getInts

-- | n行の ByteString を読み込む
getLines :: Int -> IO [BC.ByteString]
getLines n = replicateM n BC.getLine

-- | 標準入力から空白区切り String リストを読み込む
getStrings :: IO [String]
getStrings = map BC.unpack . BC.words <$> BC.getLine

-- | 標準入力から Int のペアを1つ読み込む
getPairInt :: IO (Int, Int)
getPairInt = (\[a, b] -> (a, b)) <$> getInts

-- | 標準入力から Int の3つ組を1つ読み込む
getTripleInt :: IO (Int, Int, Int)
getTripleInt = (\[a, b, c] -> (a, b, c)) <$> getInts

-- | 文字を数字（0-9）に変換する
--
-- >>> charToDigit '5'
-- 5
-- >>> charToDigit '0'
-- 0
charToDigit :: Char -> Int
charToDigit c = ord c - ord '0'

-- | ByteString の1行から整数リストを解析する
--
-- >>> parseLineIntList (BC.pack "1 2 3")
-- [1,2,3]
parseLineIntList :: BC.ByteString -> [Int]
parseLineIntList = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace)

-- | n行の Int ペアを Vector として読み込む
readIntPairIntLineV :: Int -> IO (V.Vector (Int, Int))
readIntPairIntLineV n = V.fromList <$> replicateM n getPairInt -- readPairInt → getPairInt

-- | n行の Int ペアを Unboxed Vector として読み込む
readIntPairIntLineVU :: Int -> IO (VU.Vector (Int, Int))
readIntPairIntLineVU n = VU.fromList <$> replicateM n getPairInt -- readPairInt → getPairInt

-- | 標準入力から Double リストを1行読み込む
getDouble :: IO [Double]
getDouble = map read . words . BC.unpack <$> BC.getLine

-- others

-- | 2要素リストをタプルに変換する
--
-- >>> fromListToTuple [3, 7]
-- (3,7)
fromListToTuple :: [Int] -> (Int, Int)
fromListToTuple [a, b] = (a, b)

-- | "文字列 整数" の形式の文字列をパースする
--
-- >>> readTuple "abc 42"
-- ("abc",42)
readTuple :: String -> (String, Int)
readTuple input = (str, read num :: Int)
  where
    [str, num] = words input

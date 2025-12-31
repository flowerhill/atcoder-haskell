{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Input
  ( getInt,
    getInts,
    getInteger,
    getIntegers,
    getWords,
    getGrid,
    getLines,
  )
where

import Control.Monad (replicateM)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | 1行から整数を1つ読み込む
getInt :: IO Int
getInt = readLn @Int

-- | 1行から整数を複数読み込む（空白区切り）
getInts :: IO [Int]
getInts = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

-- | 1行からInteger型を1つ読み込む
getInteger :: IO Integer
getInteger = readLn @Integer

-- | 1行からInteger型を複数読み込む（空白区切り）
getIntegers :: IO [Integer]
getIntegers = L.unfoldr (BC.readInteger . BC.dropWhile C.isSpace) <$> BC.getLine

-- | 1行から文字列を複数読み込む（空白区切り）
getWords :: IO [BC.ByteString]
getWords = BC.words <$> BC.getLine

-- | n行のグリッド（整数）を読み込む
getGrid :: Int -> IO [[Int]]
getGrid n = replicateM n getInts

-- | n行の文字列を読み込む
getLines :: Int -> IO [BC.ByteString]
getLines n = replicateM n BC.getLine

getStrings :: IO [String]
getStrings = map BC.unpack . BC.words <$> BC.getLine

getPairInt :: IO (Int, Int)
getPairInt = (\[a, b] -> (a, b)) <$> getInts

charToDigit :: Char -> Int
charToDigit c = ord c - ord '0'

parseLineIntList :: BC.ByteString -> [Int]
parseLineIntList = L.unfoldr (BC.readInt . BC.dropWhile C.isSpace)

readIntPairIntLineV :: Int -> IO (V.Vector (Int, Int))
readIntPairIntLineV n = V.fromList <$> replicateM n getPairInt -- readPairInt → getPairInt

readIntPairIntLineVU :: Int -> IO (VU.Vector (Int, Int))
readIntPairIntLineVU n = VU.fromList <$> replicateM n getPairInt -- readPairInt → getPairInt

-- Double
getDouble :: IO [Double]
getDouble = map read . words . BC.unpack <$> BC.getLine

-- others
fromListToTuple :: [Int] -> (Int, Int)
fromListToTuple [a, b] = (a, b)

readTuple :: String -> (String, Int)
readTuple input = (str, read num :: Int)
  where
    [str, num] = words input

withInTime :: Int -> Int -> Int -> Bool
withInTime start end time -- end 引数を追加
  | start <= end = time >= start && time < end
  | otherwise = time >= start || time < end

withInTimeDiff :: Int -> Int -> Int -> Bool
withInTimeDiff start diff time
  | start <= end = time >= start && time < end
  | otherwise = time >= start || time < end
  where
    end = (start + diff) `mod` 24

printYn :: Bool -> IO ()
printYn f = putStrLn $ bool "No" "Yes" f

printList :: (Show a) => [a] -> IO ()
printList lst = putStrLn $ unwords $ map show lst
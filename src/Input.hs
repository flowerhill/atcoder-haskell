{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

-- | 1行から整数を1つ読み込む
getInt :: IO Int
getInt = fst . fromJust . BS.readInt <$> BS.getLine

-- | 1行から整数を複数読み込む（空白区切り）
getInts :: IO [Int]
getInts = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

-- | 1行からInteger型を1つ読み込む
getInteger :: IO Integer
getInteger = fst . fromJust . BS.readInteger <$> BS.getLine

-- | 1行からInteger型を複数読み込む（空白区切り）
getIntegers :: IO [Integer]
getIntegers = map (fst . fromJust . BS.readInteger) . BS.words <$> BS.getLine

-- | 1行から文字列を複数読み込む（空白区切り）
getWords :: IO [BS.ByteString]
getWords = BS.words <$> BS.getLine

-- | n行のグリッド（整数）を読み込む
getGrid :: Int -> IO [[Int]]
getGrid n = replicateM n getInts

-- | n行の文字列を読み込む
getLines :: Int -> IO [BS.ByteString]
getLines n = replicateM n BS.getLine
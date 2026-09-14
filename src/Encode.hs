module Encode where

import Data.Char (isDigit)
import qualified Data.List.NonEmpty as NE

-- | ランレングス圧縮: 連続する同じ要素を (値, 個数) にまとめる
--
-- >>> runLengthEncode "aaabbc"
-- [('a',3),('b',2),('c',1)]
-- >>> runLengthEncode [1,1,2,3,3,3 :: Int]
-- [(1,2),(2,1),(3,3)]
-- >>> runLengthEncode ""
-- []
runLengthEncode :: (Eq a) => [a] -> [(a, Int)]
runLengthEncode = map (\g -> (NE.head g, NE.length g)) . NE.group

-- | ランレングス圧縮の復元: (値, 個数) の並びを元の列に戻す（'runLengthEncode' の逆）
--
-- >>> runLengthDecode [('a',3),('b',2),('c',1)]
-- "aaabbc"
-- >>> runLengthDecode ([] :: [(Int, Int)])
-- []
runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode = concatMap (\(x, n) -> replicate n x)

-- | ランレングス圧縮の結果を「個数 + 文字」を並べた文字列で表す
--
-- >>> encodeToString "aaabbc"
-- "3a2b1c"
-- >>> encodeToString ""
-- ""
encodeToString :: String -> String
encodeToString = concatMap (\(c, n) -> show n ++ [c]) . runLengthEncode

-- | 「個数 + 文字」を並べた文字列から元の文字列に戻す（'encodeToString' の逆）
--
-- >>> decodeFromString "3a2b1c"
-- "aaabbc"
-- >>> decodeFromString "12x"
-- "xxxxxxxxxxxx"
-- >>> decodeFromString ""
-- ""
decodeFromString :: String -> String
decodeFromString = runLengthDecode . parseEncoded
  where
    parseEncoded :: String -> [(Char, Int)]
    parseEncoded str = case span isDigit str of
      (nums, c : rest) -> (c, read nums) : parseEncoded rest
      _ -> []

module Encode where

import Data.List (group)

-- | エンコード: 文字列をラン長圧縮する
--
-- >>> encode "aaabbc"
-- [(3,'a'),(2,'b'),(1,'c')]
-- >>> encode ""
-- []
encode :: String -> [(Int, Char)]
encode = map (\xs -> (length xs, head xs)) . group

-- | デコード: 圧縮された形式から元の文字列に戻す
--
-- >>> decode [(3,'a'),(2,'b'),(1,'c')]
-- "aaabbc"
-- >>> decode []
-- ""
decode :: [(Int, Char)] -> String
decode = concatMap (uncurry replicate)

-- | エンコードの結果を文字列として表現
--
-- >>> encodeToString "aaabbc"
-- "3a2b1c"
encodeToString :: String -> String
encodeToString = concatMap (\(n, c) -> show n ++ [c]) . encode

-- | 文字列からデコード
--
-- >>> decodeFromString "3a2b1c"
-- "aaabbc"
-- >>> decodeFromString ""
-- ""
decodeFromString :: String -> String
decodeFromString "" = ""
decodeFromString s = decode $ parseEncoded s
  where
    parseEncoded :: String -> [(Int, Char)]
    parseEncoded [] = []
    parseEncoded str =
      let (nums, rest) = span isDigit str
       in case rest of
            (c : rs) -> (read nums, c) : parseEncoded rs
            [] -> []

    isDigit c = c >= '0' && c <= '9'
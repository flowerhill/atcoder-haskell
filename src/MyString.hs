-- | 文字列操作（回文判定・部分文字列）まわりのユーティリティ。
module MyString where

import qualified Data.List as L

-- | 文字列が回文か判定する
--
-- >>> isPalindrome "aba"
-- True
-- >>> isPalindrome "abc"
-- False
-- >>> isPalindrome ""
-- True
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- | 長さkの部分文字列に回文が含まれるか判定する
--
-- >>> containsPalindrome 3 "abcba"
-- True
-- >>> containsPalindrome 3 "abcde"
-- False
containsPalindrome :: Int -> String -> Bool
containsPalindrome k s = or [s' == reverse s' | s' <- substringK k s]

-- | 整数が回文数か判定する
--
-- >>> isPalindromeInt 121
-- True
-- >>> isPalindromeInt 123
-- False
isPalindromeInt :: Int -> Bool
isPalindromeInt n =
  if even numDigits
    then leftHalf == reverse rightHalf
    else leftHalf == reverse (tail rightHalf)
  where
    str = show n
    numDigits = length str
    (leftHalf, rightHalf) = splitAt (numDigits `div` 2) str

-- | 部分文字列の出現回数を返す
--
-- >>> countSubstring "ab" "ababab"
-- 3
-- >>> countSubstring "xyz" "abcabc"
-- 0
countSubstring :: String -> String -> Int
countSubstring sub str = length $ filter (L.isPrefixOf sub) $ L.tails str

-- | 部分文字列の出現位置インデックスリストを返す
--
-- >>> findSubstringIndices "ab" "ababab"
-- [0,2,4]
-- >>> findSubstringIndices "xy" "abcabc"
-- []
findSubstringIndices :: String -> String -> [Int]
findSubstringIndices sub str = [i | (i, s) <- zip [0 ..] (L.tails str), sub `L.isPrefixOf` s]

-- | 位置 start から len 文字の部分文字列を返す
--
-- >>> substring 1 3 "abcdef"
-- "bcd"
-- >>> substring 0 2 "hello"
-- "he"
substring :: Int -> Int -> String -> String
substring start len str = take len (drop start str)

-- | 長さk の全部分文字列リストを返す
--
-- >>> substringK 2 "abcd"
-- ["ab","bc","cd"]
substringK :: Int -> String -> [String]
substringK k s = [substring i k s | i <- [0 .. length s - k]]

-- | 全長さの部分文字列を列挙する
--
-- >>> substrings "abc"
-- ["a","b","c","ab","bc","abc"]
substrings :: String -> [String]
substrings s = concat [substringK i s | i <- [1 .. length s]]

-- | 文字列のi番目の文字をt[i]に変更する
--
-- >>> changeChar "abc" "xyz" 1
-- "ayc"
-- >>> changeChar "abc" "xyz" 0
-- "xbc"
changeChar :: String -> String -> Int -> String
changeChar s t i = take i s ++ [t !! i] ++ drop (i + 1) s

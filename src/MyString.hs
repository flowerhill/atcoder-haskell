-- | 文字列操作（回文判定・部分文字列）まわりのユーティリティ。
--
-- 内部表現は @VU.Vector Char@ に統一している。String のままだと
-- @take len (drop start s)@ が毎回 O(start + len) かかり、位置をずらして
-- 全部分文字列を舐める処理が O(N^2) に化けるが、Vector なら slice が O(1) で
-- 取れるので出力量に比例した計算量で済む。
module MyString where

import qualified Data.Vector.Unboxed as VU

-- | String を Unboxed Vector Char に変換する（内部表現への入口）
--
-- >>> VU.toList (toVec "abc")
-- "abc"
toVec :: String -> VU.Vector Char
toVec = VU.fromList
{-# INLINE toVec #-}

-- | Vector Char が回文か判定する。前後から突き合わせ、中央で打ち切る。
-- reverse を作らないので不一致が early return で効く。
--
-- >>> isPalindromeV (toVec "abcba")
-- True
-- >>> isPalindromeV (toVec "abca")
-- False
-- >>> isPalindromeV (toVec "")
-- True
isPalindromeV :: VU.Vector Char -> Bool
isPalindromeV v = all (\i -> VU.unsafeIndex v i == VU.unsafeIndex v (n - 1 - i)) [0 .. n `div` 2 - 1]
  where
    n = VU.length v

-- | 文字列が回文か判定する
--
-- >>> isPalindrome "aba"
-- True
-- >>> isPalindrome "abc"
-- False
-- >>> isPalindrome ""
-- True
isPalindrome :: String -> Bool
isPalindrome = isPalindromeV . toVec

-- | 長さkの部分文字列に回文が含まれるか判定する。O(N*k)
--
-- 各開始位置で slice を取って回文判定するだけ。String を切り出さないので
-- 見つかった時点で打ち切れる。
--
-- >>> containsPalindrome 3 "abcba"
-- True
-- >>> containsPalindrome 3 "abcde"
-- False
-- >>> containsPalindrome 9 "abc"
-- False
containsPalindrome :: Int -> String -> Bool
containsPalindrome k s
  -- 長さ 0 の部分文字列は空文字列＝回文なので常に True
  | k <= 0 = True
  | otherwise = any (\i -> isPalindromeV (VU.slice i k v)) [0 .. VU.length v - k]
  where
    v = toVec s

-- | 整数が回文数か判定する
--
-- >>> isPalindromeInt 121
-- True
-- >>> isPalindromeInt 123
-- False
-- >>> isPalindromeInt 1221
-- True
isPalindromeInt :: Int -> Bool
isPalindromeInt = isPalindrome . show

-- | 部分文字列の出現回数を返す
--
-- >>> countSubstring "ab" "ababab"
-- 3
-- >>> countSubstring "xyz" "abcabc"
-- 0
countSubstring :: String -> String -> Int
countSubstring sub str = length (findSubstringIndices sub str)

-- | 部分文字列の出現位置インデックスリストを返す。O(N*M)
--
-- tails を作って isPrefixOf する代わりに、各位置の slice と直接比較する。
-- Vector の (==) は要素ごとの比較で不一致なら即打ち切る。
--
-- >>> findSubstringIndices "ab" "ababab"
-- [0,2,4]
-- >>> findSubstringIndices "xy" "abcabc"
-- []
findSubstringIndices :: String -> String -> [Int]
findSubstringIndices sub str = [i | i <- [0 .. VU.length v - m], VU.slice i m v == u]
  where
    v = toVec str
    u = toVec sub
    m = VU.length u

-- | 位置 start から len 文字の部分文字列を返す。
-- take/drop 版と同じく、範囲からはみ出す分は切り詰める。
--
-- >>> substring 1 3 "abcdef"
-- "bcd"
-- >>> substring 0 2 "hello"
-- "he"
-- >>> substring 4 10 "abcdef"
-- "ef"
-- >>> substring 9 2 "abcdef"
-- ""
substring :: Int -> Int -> String -> String
substring start len str = VU.toList $ VU.slice from cnt v
  where
    v = toVec str
    n = VU.length v
    from = max 0 (min n start)
    cnt = max 0 (min (n - from) len)

-- | 長さk の全部分文字列リストを返す。O(N*k)
--
-- >>> substringK 2 "abcd"
-- ["ab","bc","cd"]
-- >>> substringK 5 "abcd"
-- []
substringK :: Int -> String -> [String]
substringK k s
  | k < 0 = error $ "substringK: negative length k=" ++ show k ++ ", |s|=" ++ show (VU.length v)
  | otherwise = [VU.toList (VU.slice i k v) | i <- [0 .. VU.length v - k]]
  where
    v = toVec s

-- | 全長さの部分文字列を列挙する。O(N^3)（出力量そのもの）
--
-- >>> substrings "abc"
-- ["a","b","c","ab","bc","abc"]
substrings :: String -> [String]
substrings s = [VU.toList (VU.slice i k v) | k <- [1 .. n], i <- [0 .. n - k]]
  where
    v = toVec s
    n = VU.length v

-- | 文字列のi番目の文字をt[i]に変更する
--
-- >>> changeChar "abc" "xyz" 1
-- "ayc"
-- >>> changeChar "abc" "xyz" 0
-- "xbc"
changeChar :: String -> String -> Int -> String
changeChar s t i
  | i < 0 || i >= VU.length v || i >= VU.length u =
      error $
        "changeChar: index out of range: i=" ++ show i ++ ", |s|=" ++ show (VU.length v) ++ ", |t|=" ++ show (VU.length u)
  | otherwise = VU.toList $ v VU.// [(i, VU.unsafeIndex u i)]
  where
    v = toVec s
    u = toVec t

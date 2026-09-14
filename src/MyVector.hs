module MyVector where

import Data.List (scanl')
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as VU

-- 方針:
--   vector に既にある操作は再実装しない。
--   ここには不変ベクタ（VU.Vector）を受け取って返すアルゴリズムを置く。
--   可変ベクタ（VUM）を使うものは MyMVector に置く。

{-- 部分列 --}

-- | 各位置 i について「i 番目で終わる狭義単調増加部分列の最長の長さ」を返す（O(N log N)）。
-- 最大値を取れば LIS の長さになる。
-- 「i 番目から始まる狭義減少部分列の長さ」は @VU.reverse . lisEnding . VU.reverse@ で得られる。
--
-- ends（長さ k の増加部分列の末尾として最小の値を、k の小さい順に並べた集合）を更新し、
-- y が ends の何番目に入ったか（= y 未満の末尾の個数）+ 1 を長さとする。
-- Set は unboxed vector に載らないので、ends の更新はリスト上の scanl' で行う。
-- 結果を unboxed vector に詰めることで各長さがその場で確定し、過去の Set を掴んだ
-- サンクが残らない（リストのまま返すと N = 3×10^5 で数百 MB に膨らむ）。
--
-- >>> lisEnding (VU.fromList [1,2,3,3,2,1 :: Int])
-- [1,2,3,3,2,1]
-- >>> lisEnding (VU.fromList [2,5,3,4,1 :: Int])
-- [1,2,2,3,1]
-- >>> lisEnding (VU.fromList [3,3,3 :: Int])
-- [1,1,1]
-- >>> lisEnding (VU.empty :: VU.Vector Int)
-- []
lisEnding :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
lisEnding ys = VU.fromListN (VU.length ys) $ zipWith (\y s -> S.findIndex y s + 1) xs (drop 1 $ scanl' push S.empty xs)
  where
    xs = VU.toList ys
    -- y 以上の最小要素を y で置き換える（無ければ末尾に追加 = 最長が 1 伸びる）
    push ends y = S.insert y $ maybe ends (`S.delete` ends) (S.lookupGE y ends)

-- | 'lisEnding' の広義版。各位置 i について「i 番目で終わる広義単調増加部分列の最長の長さ」を返す。
-- (値, 添字) の組は後ろほど添字が大きいので、組での狭義増加 = 値での広義増加 になる。
--
-- >>> lisEndingNonStrict (VU.fromList [1,2,3,3,2,1 :: Int])
-- [1,2,3,4,3,2]
-- >>> lisEndingNonStrict (VU.fromList [3,3,3 :: Int])
-- [1,2,3]
-- >>> lisEndingNonStrict (VU.empty :: VU.Vector Int)
-- []
lisEndingNonStrict :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
lisEndingNonStrict = lisEnding . VU.imap (\i y -> (y, i))

module LIS where

import BSearch (bisectM)
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

{-- 最長増加部分列 / 最長減少部分列 --}

-- | 各位置 i で終わる最長増加部分列（狭義）の長さを返す (O(N log N))。
-- 最大値を取れば LIS の長さになる。
--
-- >>> lisLengths (VU.fromList [3,1,4,1,5,9,2,6])
-- [1,1,2,1,3,4,2,4]
-- >>> lisLengths (VU.fromList [5,4,3,2,1])
-- [1,1,1,1,1]
-- >>> lisLengths (VU.fromList [3,3,3])
-- [1,1,1]
-- >>> lisLengths VU.empty
-- []
lisLengths :: VU.Vector Int -> VU.Vector Int
lisLengths = lisLengthsBy (>=)

-- | 'lisLengths' の広義版。各位置 i で終わる最長の広義単調増加部分列の長さを返す。
--
-- >>> lisLengthsNonStrict (VU.fromList [1,2,3,3,2,1])
-- [1,2,3,4,3,2]
-- >>> lisLengthsNonStrict (VU.fromList [3,3,3])
-- [1,2,3]
lisLengthsNonStrict :: VU.Vector Int -> VU.Vector Int
lisLengthsNonStrict = lisLengthsBy (>)

-- | 各位置 i から始まる最長減少部分列（狭義）の長さを返す (O(N log N))。
-- 反転して LIS を取り、結果を反転して戻す。
--
-- >>> ldsLengths (VU.fromList [3,1,4,1,5,9,2,6])
-- [2,1,2,1,2,2,1,1]
-- >>> ldsLengths (VU.fromList [1,2,3,3,2,1])
-- [1,2,3,3,2,1]
ldsLengths :: VU.Vector Int -> VU.Vector Int
ldsLengths = VU.reverse . lisLengths . VU.reverse

-- | 'lisLengths' と 'lisLengthsNonStrict' の共通実装。
-- patience sorting の tails（長さ k の増加部分列の末尾として最小の値）を MVector で持ち、
-- tails[0..len) のうち @t `cmp` x@ を初めて満たす位置 pos に x を置く（無ければ末尾 = 最長が 1 伸びる）。
-- cmp が (>=) なら同じ値の位置に上書きされるので狭義、(>) なら同じ値の後ろに置かれるので広義になる。
-- 現在長 len は foldM で引き回すので可変セルは不要。
--
-- >>> lisLengthsBy (>=) (VU.fromList [2,5,3,4,1])
-- [1,2,2,3,1]
lisLengthsBy :: (Int -> Int -> Bool) -> VU.Vector Int -> VU.Vector Int
lisLengthsBy cmp xs = runST $ do
  tails <- VUM.replicate n (maxBound :: Int)
  result <- VUM.new n
  let step len i = do
        let x = xs VU.! i
        (_, pos) <- bisectM (-1, len) $ \mid -> (`cmp` x) <$> VUM.read tails mid
        VUM.write tails pos x
        VUM.write result i (pos + 1)
        return $ max len (pos + 1)
  _ <- foldM step 0 [0 .. n - 1]
  VU.unsafeFreeze result
  where
    n = VU.length xs

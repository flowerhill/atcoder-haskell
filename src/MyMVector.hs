{-# LANGUAGE FlexibleContexts #-}

module MyMVector where

import BSearchVector (bisectM)
import Control.Monad (foldM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- 方針:
--   swap / modify / grow / slice は VUM に既にあるので再実装しない。
--   ここには「vector に無い安全アクセサ」と「自前の可変ベクタ・アルゴリズム」を置く。

{-- 安全アクセサ（範囲外で例外を投げず Maybe/Bool を返す） --}

-- | 範囲内なら Just で読む（範囲外は Nothing）。ST/IO 両対応。
--
-- >>> import qualified Data.Vector.Unboxed.Mutable as VUM
-- >>> v <- VUM.generate 3 id :: IO (VUM.IOVector Int)
-- >>> v !? 1
-- Just 1
-- >>> v !? 5
-- Nothing
(!?) :: (PrimMonad m, VU.Unbox e) => VUM.MVector (PrimState m) e -> Int -> m (Maybe e)
(!?) v i
  | i < 0 || i >= VUM.length v = return Nothing
  | otherwise = Just <$> VUM.unsafeRead v i
{-# INLINE (!?) #-}

-- | (!?) の別名（命名を MyMArray と揃えたいとき用）
safeReadM :: (PrimMonad m, VU.Unbox e) => VUM.MVector (PrimState m) e -> Int -> m (Maybe e)
safeReadM = (!?)
{-# INLINE safeReadM #-}

-- | 範囲内なら書いて True、範囲外なら False を返す。
--
-- >>> import qualified Data.Vector.Unboxed.Mutable as VUM
-- >>> v <- VUM.replicate 3 (0 :: Int) :: IO (VUM.IOVector Int)
-- >>> ok <- safeWriteM v 1 99
-- >>> x <- VUM.read v 1
-- >>> (ok, x)
-- (True,99)
-- >>> safeWriteM v 5 99
-- False
safeWriteM :: (PrimMonad m, VU.Unbox e) => VUM.MVector (PrimState m) e -> Int -> e -> m Bool
safeWriteM v i x
  | i < 0 || i >= VUM.length v = return False
  | otherwise = True <$ VUM.unsafeWrite v i x
{-# INLINE safeWriteM #-}

{-- LIS / LDS （VU版。MyMArray の UArray 版と対になる）--}
-- NOTE: 同等品が Util.hs にもある。重複が気になるなら片方へ寄せること。

-- | 各位置 i で終わる最長増加部分列（狭義）の長さを返す (O(N log N))。
-- patience sorting の tails を MVector で持ち、各要素で lower bound を二分探索する。
-- 現在長 len は foldM で引き回すので可変セルは不要。
--
-- >>> lisLengths (VU.fromList [3,1,4,1,5,9,2,6])
-- [1,1,2,1,3,4,2,4]
-- >>> lisLengths (VU.fromList [5,4,3,2,1])
-- [1,1,1,1,1]
lisLengths :: VU.Vector Int -> VU.Vector Int
lisLengths xs = runST $ do
  tails <- VUM.replicate n (maxBound :: Int)
  result <- VUM.new n
  let step len i = do
        let x = xs VU.! i
        -- tails[0..len) のうち x 以上が初めて現れる位置 pos（無ければ len）
        (_, pos) <- bisectM (-1, len) $ \mid -> (>= x) <$> VUM.read tails mid
        VUM.write tails pos x
        VUM.write result i (pos + 1)
        return $ max len (pos + 1)
  _ <- foldM step 0 [0 .. n - 1]
  VU.unsafeFreeze result
  where
    n = VU.length xs

-- | 各位置 i から始まる最長減少部分列（狭義）の長さを返す (O(N log N))。
-- 反転して LIS を取り、結果を反転して戻す（VU.reverse 一発で済む）。
--
-- >>> ldsLengths (VU.fromList [3,1,4,1,5,9,2,6])
-- [2,1,2,1,2,2,1,1]
ldsLengths :: VU.Vector Int -> VU.Vector Int
ldsLengths = VU.reverse . lisLengths . VU.reverse

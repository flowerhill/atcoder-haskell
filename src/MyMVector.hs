{-# LANGUAGE FlexibleContexts #-}

module MyMVector where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- 方針:
--   swap / modify / grow / slice は VUM に既にあるので再実装しない。
--   ここには vector に無い安全アクセサを置く。
--   可変ベクタを内部で使うアルゴリズムは、話題ごとのモジュール（LIS など）に置く。

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

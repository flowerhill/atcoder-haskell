{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | 法を型に載せた剰余演算。素の整数演算は "Math" を参照。
--
-- 設計は ac-library-hs の @AtCoder.ModInt@ に倣っている:
-- 値は 'Word32' で持ち、積のときだけ 'Word64' に広げる。加減算は剰余を取らず
-- 条件分岐で補正する。法は 'Proxy#' 経由で型から取り、全メソッドを INLINE して
-- 定数に畳ませる。
--
-- 法 p は 2^31 未満であることを前提とする（減算のアンダーフロー補正がこれに依存する）。
module ModInt
  ( IntMod,
    ModInt998244353,
    ModInt1000000007,
    modVal,
    toIntMod,
    unIntMod,
    powIntMod,
    invIntMod,
    sumOfGeoMod,
    sumOfArithMod,
    runIntMod,
  )
where

import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal', someNatVal)
import Math (exEuclid, sumOfRange)

-- | 法 p の剰余環の元。内部表現は常に [0, p) に正規化された 'Word32'。
newtype IntMod (p :: Nat) = IntMod Word32
  deriving newtype (Eq, Ord)

type ModInt998244353 = IntMod 998244353

type ModInt1000000007 = IntMod 1000000007

-- | 提出時はそのまま print したいので、生の整数として表示する。
--
-- >>> show (toIntMod 1000000000 :: ModInt998244353)
-- "1755647"
instance Show (IntMod p) where
  show = show . unIntMod
  {-# INLINE show #-}

-- | 型レベルの法を 'Int' として取り出す。
--
-- >>> modVal (toIntMod 0 :: ModInt998244353)
-- 998244353
modVal :: forall p. (KnownNat p) => IntMod p -> Int
modVal _ = fromIntegral (natVal' (proxy# @p))
{-# INLINE modVal #-}

-- | 法を 'Word32' として取り出す内部用。
modVal32# :: forall p. (KnownNat p) => Proxy# p -> Word32
modVal32# p = fromIntegral (natVal' p)
{-# INLINE modVal32# #-}

instance (KnownNat p) => Num (IntMod p) where
  -- x1 + x2 < 2p < 2^32 なので Word32 のまま足して、溢れた分を 1 回引けばよい
  IntMod x1 + IntMod x2
    | z >= m = IntMod (z - m)
    | otherwise = IntMod z
    where
      !z = x1 + x2
      !m = modVal32# (proxy# @p)
  {-# INLINE (+) #-}

  -- x1 < x2 のとき z は 2^32 へ回り込む。p < 2^31 なら必ず z >= p になるので、
  -- m を足してもう一度回り込ませると正しい値に戻る
  IntMod x1 - IntMod x2
    | z >= m = IntMod (z + m)
    | otherwise = IntMod z
    where
      !z = x1 - x2
      !m = modVal32# (proxy# @p)
  {-# INLINE (-) #-}

  IntMod x1 * IntMod x2 = IntMod (fromIntegral (z `rem` m))
    where
      !z = fromIntegral x1 * fromIntegral x2 :: Word64
      !m = fromIntegral (natVal' (proxy# @p)) :: Word64
  {-# INLINE (*) #-}

  negate x = 0 - x
  {-# INLINE negate #-}

  abs = id
  {-# INLINE abs #-}

  signum (IntMod x) = IntMod (if x == 0 then 0 else 1)
  {-# INLINE signum #-}

  -- 'Integer' を経由するのでホットパスでは使わない。'Int' からは toIntMod を使うこと
  fromInteger n = IntMod (fromInteger (n `mod` toInteger (natVal' (proxy# @p))))
  {-# INLINE fromInteger #-}

-- | 逆元を使った除算。法と互いに素でない数で割ると 'invIntMod' が 'error' で落ちる。
--
-- >>> toIntMod 1000000000 / toIntMod 3 :: ModInt998244353
-- 666081451
instance (KnownNat p) => Fractional (IntMod p) where
  recip = invIntMod
  {-# INLINE recip #-}
  fromRational r = error $ "IntMod.fromRational: not supported: " ++ show r

-- | 'Int' から持ち上げる。負の数や法を超える数もそのまま渡せる。
-- 'fromIntegral' は 'Integer' を経由して遅いので、'Int' からは必ずこちらを使う。
--
-- >>> toIntMod 1000000000 :: ModInt998244353
-- 1755647
-- >>> toIntMod (-1) :: ModInt998244353
-- 998244352
toIntMod :: forall p. (KnownNat p) => Int -> IntMod p
toIntMod x = IntMod (fromIntegral (x `mod` fromIntegral (natVal' (proxy# @p))))
{-# INLINE toIntMod #-}

-- | 生の 'Int' に戻す。
--
-- >>> unIntMod (toIntMod 5 :: ModInt998244353)
-- 5
unIntMod :: IntMod p -> Int
unIntMod (IntMod x) = fromIntegral x
{-# INLINE unIntMod #-}

-- | 繰り返し二乗法。指数が負なら 'error'。
--
-- >>> powIntMod (toIntMod 2) 100 :: ModInt998244353
-- 882499718
-- >>> powIntMod (toIntMod 2) 0 :: ModInt998244353
-- 1
powIntMod :: (KnownNat p) => IntMod p -> Int -> IntMod p
powIntMod b e
  | e < 0 = error $ "powIntMod: negative exponent " ++ show e
  | otherwise = go b e 1
  where
    go _ 0 !acc = acc
    go !x !n !acc = go (x * x) (n `div` 2) (if odd n then acc * x else acc)
{-# INLINE powIntMod #-}

-- | 拡張ユークリッドによる逆元。法が素数である必要はなく、法と互いに素であればよい。
-- 互いに素でなければ 'error'。
--
-- >>> invIntMod (toIntMod 3) :: ModInt998244353
-- 332748118
-- >>> invIntMod (toIntMod 3) * toIntMod 3 :: ModInt998244353
-- 1
invIntMod :: forall p. (KnownNat p) => IntMod p -> IntMod p
invIntMod x = case exEuclid v m of
  (1, s, _) -> toIntMod s
  (-1, s, _) -> toIntMod (-s)
  (g, _, _) -> error $ "invIntMod: " ++ show v ++ " is not coprime to " ++ show m ++ " (gcd = " ++ show g ++ ")"
  where
    v = unIntMod x
    m = fromIntegral (natVal' (proxy# @p))

-- | 等比数列の和: 初項 a, 公比 r, 項数 n。
-- a * (r^n - 1) / (r - 1) を計算する。r == 1 では分母が 0 になるので先に分岐する。
--
-- >>> sumOfGeoMod 1 2 4 :: ModInt998244353
-- 15
-- >>> sumOfGeoMod 3 3 3 :: ModInt998244353
-- 39
-- >>> sumOfGeoMod 1 1 5 :: ModInt998244353
-- 5
sumOfGeoMod :: (KnownNat p) => IntMod p -> IntMod p -> Int -> IntMod p
sumOfGeoMod a r n
  | r == 1 = a * toIntMod n
  | otherwise = a * (powIntMod r n - 1) / (r - 1)

-- | 等差数列の和: a から b までの公差 1 の整数の総和。
--
-- >>> sumOfArithMod 1 10 :: ModInt998244353
-- 55
-- >>> sumOfArithMod 1 100000 :: ModInt998244353
-- 8828235
-- >>> sumOfArithMod 1 100000 :: ModInt1000000007
-- 49965
sumOfArithMod :: (KnownNat p) => Integer -> Integer -> IntMod p
sumOfArithMod a b = fromInteger (sumOfRange a b)

-- | 法が入力で与えられる問題向け。実行時の 'Int' を型レベルに持ち上げて計算する。
-- ac-library-hs には無い（本家 C++ の @dynamic_modint@ も移植時に落とされている）。
-- 法が定数に畳まれなくなるぶん 2 倍ほど遅いので、法が問題文で固定なら使わないこと。
--
-- >>> runIntMod 10 (\_ -> toIntMod 7 * toIntMod 8)
-- 6
-- >>> runIntMod 998244353 (\_ -> powIntMod (toIntMod 2) 100)
-- 882499718
runIntMod :: Int -> (forall p. (KnownNat p) => Proxy p -> IntMod p) -> Int
runIntMod m f = case someNatVal (fromIntegral m) of
  SomeNat (_ :: Proxy p) -> unIntMod (f (Proxy @p))

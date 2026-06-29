-- | DEBUG 環境変数で制御するトレース用ユーティリティ。
module Debug where

import Debug.Trace (traceShow)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | DEBUG環境変数が設定されていれば値を stderr に出力し () を返す
--
-- >>> dbg (42 :: Int)
-- ()
dbg :: (Show a) => a -> ()
dbg = case getDebugEnv of
  Just _ -> (`traceShow` ())
  Nothing -> const ()

-- | DEBUG環境変数の値を取得する
getDebugEnv :: Maybe String
getDebugEnv = unsafePerformIO (lookupEnv "DEBUG")
{-# NOINLINE getDebugEnv #-}

-- | 値を stderr に出力しながら同じ値を返す（デバッグ用）
--
-- >>> traceDbg (42 :: Int)
-- 42
-- 42
traceDbg :: (Show b) => b -> b
traceDbg itm = traceShow itm itm

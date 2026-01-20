{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lib (solve) where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Array.IArray qualified as IA
import Data.Array.ST.Safe
import Data.IntSet qualified as IS
import Data.List
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

-- | 問題固有のロジック
-- この関数を書き換えて問題を解く
solve :: Int -> [Int] -> Int
solve n xs = sum xs

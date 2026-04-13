module MyArray where

import Data.Array.IArray

{-- IArray用 --}

-- | 条件を満たす要素のインデックスリストを返す
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 4) [1, 3, 2, 5, 4] :: Array Int Int
-- >>> findArrayIndices (> 2) arr
-- [1,3,4]
findArrayIndices :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> [i]
findArrayIndices predicate as = [i | (i, e) <- assocs as, predicate e]

-- | Arrayの値リストを返す
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (1, 3) [10, 20, 30] :: Array Int Int
-- >>> getArrayValues arr
-- [10,20,30]
getArrayValues :: (IArray a1 a2, Ix i) => a1 i a2 -> [a2]
getArrayValues as = [e | (_, e) <- assocs as]

-- | Arrayのキーリストを返す
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (1, 3) [10, 20, 30] :: Array Int Int
-- >>> getArrayKeys arr
-- [1,2,3]
getArrayKeys :: (IArray a1 e, Ix a2) => a1 a2 e -> [a2]
getArrayKeys as = [k | (k, _) <- assocs as]

-- | 範囲内なら Just、範囲外なら Nothing を返す (!?)
-- (safeRead と同等の機能を演算子として提供)
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 2) [10, 20, 30] :: Array Int Int
-- >>> safeRead arr 1
-- Just 20
-- >>> safeRead arr 5
-- Nothing
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i =
  let b = bounds arr
   in if inRange b i
        then Just (arr ! i)
        else Nothing

-- | 範囲内なら Just で値を返す安全な読み取り
--
-- >>> import Data.Array.IArray (listArray)
-- >>> let arr = listArray (0, 2) [10, 20, 30] :: Array Int Int
-- >>> safeRead arr 2
-- Just 30
-- >>> safeRead arr (-1)
-- Nothing
safeRead :: (IArray a e, Ix i) => a i e -> i -> Maybe e
safeRead arr idx =
  if inRange (bounds arr) idx
    then Just (arr ! idx)
    else Nothing

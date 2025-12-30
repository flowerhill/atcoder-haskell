module Array where

import Data.Array.IArray

{-- IArray用 --}
-- Array用のfind
findArrayIndices :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> [i]
findArrayIndices predicate as = [i | (i, e) <- assocs as, predicate e]

-- Arrayのvalueを取得する
getArrayValues :: (IArray a1 a2, Ix i) => a1 i a2 -> [a2]
getArrayValues as = [e | (_, e) <- assocs as]

-- Arrayのkeyを取得する
getArrayKeys :: (IArray a1 e, Ix a2) => a1 a2 e -> [a2]
getArrayKeys as = [k | (k, _) <- assocs as]

-- IArray用の(!?)
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i =
  let b = bounds arr
   in if inRange b i
        then Just (arr ! i)
        else Nothing

-- IArray (immutable) 用の安全読み取り
safeRead :: (IArray a e, Ix i) => a i e -> i -> Maybe e
safeRead arr idx =
  if inRange (bounds arr) idx
    then Just (arr ! idx)
    else Nothing

module MyArray where

import Data.Array.IArray

{-- IArray用 --}

-- | 抜けのあるインデックスを既定値で埋めて Array を作る。O(範囲の大きさ + 要素数)
--
-- array は範囲内の全インデックス分の組が要り、抜けがあると Array では参照時に
-- undefined、UArray では不定値になる。accumArray で既定値の配列を作り、
-- 各組の値で上書きする。同じインデックスが複数あれば後の値が勝つ（(//) と同じ）。
-- 範囲外のインデックスを渡すと error。
--
-- >>> arrayWithDefault 0 (1, 5) [(2, 20), (4, 40)] :: Array Int Int
-- array (1,5) [(1,0),(2,20),(3,0),(4,40),(5,0)]
-- >>> arrayWithDefault '.' (0, 2) [(1, 'a'), (1, 'b')] :: Array Int Char
-- array (0,2) [(0,'.'),(1,'b'),(2,'.')]
-- >>> arrayWithDefault False ((1, 1), (2, 2)) [((2, 1), True)] :: Array (Int, Int) Bool
-- array ((1,1),(2,2)) [((1,1),False),((1,2),False),((2,1),True),((2,2),False)]
arrayWithDefault :: (IArray a e, Ix i) => e -> (i, i) -> [(i, e)] -> a i e
arrayWithDefault def = accumArray (\_ x -> x) def

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

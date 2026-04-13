module MyList where

import Control.Monad.State (StateT (StateT, runStateT))
import Data.List (find, findIndex)

{--  リスト走査 --}

-- | リストの先頭要素を安全に取得する
--
-- >>> safeHead [1,2,3 :: Int]
-- Just 1
-- >>> safeHead ([] :: [Int])
-- Nothing
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | リストのn番目の要素を安全に取得する（0始まり）
--
-- >>> safeGetElement [10,20,30 :: Int] 1
-- Just 20
-- >>> safeGetElement [10,20,30 :: Int] 5
-- Nothing
-- >>> safeGetElement [10,20,30 :: Int] (-1)
-- Nothing
safeGetElement :: [a] -> Int -> Maybe a
safeGetElement [] _ = Nothing
safeGetElement (x : xs) 0 = Just x
safeGetElement (x : xs) n
  | n < 0 = Nothing
  | otherwise = safeGetElement xs (n - 1)

-- | インデックスi以降で条件を満たす最初の位置を返す
--
-- >>> findIndexFrom 2 (> 3) [1,2,3,4,5]
-- Just 3
-- >>> findIndexFrom 0 (> 10) [1,2,3]
-- Nothing
findIndexFrom :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndexFrom i f s =
  case findIndex f (drop i s) of
    Just j -> Just (i + j)
    Nothing -> Nothing

-- | インデックスi以降でxと等しい最初の位置を返す
--
-- >>> elemIndexFrom 2 'b' "abcbc"
-- Just 3
-- >>> elemIndexFrom 0 'z' "abcbc"
-- Nothing
elemIndexFrom :: (Eq a) => Int -> a -> [a] -> Maybe Int
elemIndexFrom i x = findIndexFrom i (== x)

-- | リストから2要素の組み合わせ（重複なし）をすべて列挙する
--
-- >>> combinations [1,2,3 :: Int]
-- [(1,2),(1,3),(2,3)]
-- >>> combinations ([] :: [Int])
-- []
combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x : xs) = [(x, y) | y <- xs] ++ combinations xs

-- | リストの最大値を安全に取得する
--
-- >>> safeMaximum [3,1,4,1,5 :: Int]
-- Just 5
-- >>> safeMaximum ([] :: [Int])
-- Nothing
safeMaximum :: (Ord a) => [a] -> Maybe a
safeMaximum lst
  | null lst = Nothing
  | otherwise = Just $ maximum lst

-- | 先頭要素が条件を満たすペアを検索する
--
-- >>> findOnFst (> 2) [(1,'a'),(3,'b'),(2,'c')]
-- Just (3,'b')
-- >>> findOnFst (> 10) [(1,'a'),(3,'b')]
-- Nothing
findOnFst :: (t -> Bool) -> [(t, b)] -> Maybe (t, b)
findOnFst f = find (f . fst)

-- | 第2要素が条件を満たすペアを検索する
--
-- >>> findOnSnd (> 2) [(1,1),(2,3),(3,2 :: Int)]
-- Just (2,3)
-- >>> findOnSnd (> 10) [(1,1 :: Int),(2,2)]
-- Nothing
findOnSnd :: (Foldable t) => (b -> Bool) -> t (a, b) -> Maybe (a, b)
findOnSnd f = find (f . snd)

-- | 各リストから1要素ずつ選んだ全パターンを列挙する
--
-- >>> combinationList [[1,2],[3,4 :: Int]]
-- [[1,3],[1,4],[2,3],[2,4]]
-- >>> combinationList ([] :: [[Int]])
-- [[]]
combinationList :: [[a]] -> [[a]]
combinationList [] = [[]]
combinationList (xs : xss) = [x : ys | x <- xs, ys <- combinationList xss]

-- | 重複を許してl〜rからk個取る順列（条件付き生成）
--
-- >>> combinationsWithRepetition 1 3 2
-- [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
-- >>> combinationsWithRepetition 1 2 0
-- [[]]
combinationsWithRepetition :: Int -> Int -> Int -> [[Int]]
combinationsWithRepetition l r = go
  where
    go 0 = [[]]
    go k = [x : xs | x <- [l .. r], xs <- go (k - 1), x <= head (x : xs) || null xs]

-- | リストからn個を選ぶ組み合わせをすべて列挙する
--
-- >>> combinationsN 2 [1,2,3 :: Int]
-- [[1,2],[1,3],[2,3]]
-- >>> combinationsN 0 [1,2,3 :: Int]
-- [[]]
-- >>> combinationsN 4 [1,2,3 :: Int]
-- []
combinationsN :: Int -> [a] -> [[a]]
combinationsN _ [] = []
combinationsN n as@(_ : xs)
  | n == 0 = [[]]
  | n == 1 = map pure as
  | n == l = pure as
  | n > l = []
  | otherwise = run (l - 1) (n - 1) as $ combinationsN (n - 1) xs
  where
    l = length as

    run :: Int -> Int -> [a] -> [[a]] -> [[a]]
    run m k ys cs
      | m == k = map (ys ++) cs
      | otherwise = case take (m - k + 1) ys of
          (q : qs) -> do
            let dc = product [(m - k + 1) .. (m - 1)] `div` product [1 .. (k - 1)]
            map (q :) cs ++ run (m - 1) k qs (drop dc cs)
          [] -> error "Invalid Case"

-- | モナドを使って状態を累積しながらリストを変換する
--
-- >>> import Control.Monad.Identity (runIdentity)
-- >>> runIdentity $ mapAccumM (\s x -> return (s + x, s + x)) (0 :: Int) [1,2,3 :: Int]
-- (6,[1,3,6])
mapAccumM :: (Monad m, Traversable t) => (s -> a -> m (s, b)) -> s -> t a -> m (s, t b)
mapAccumM f s t = do
  (t', s') <-
    runStateT
      ( traverse
          ( \a ->
              StateT
                ( \s'' -> do
                    (s''', b) <- f s'' a
                    return (b, s''')
                )
          )
          t
      )
      s
  return (s', t')

-- | mapAccumM の引数順を入れ替えたバージョン
--
-- >>> import Control.Monad.Identity (runIdentity)
-- >>> runIdentity $ forAccumM (0 :: Int) [1,2,3 :: Int] (\s x -> return (s + x, x * 2))
-- (6,[2,4,6])
forAccumM :: (Monad m, Traversable t) => s -> t a -> (s -> a -> m (s, b)) -> m (s, t b)
forAccumM initial xs f = mapAccumM f initial xs

-- | リストに1始まりのインデックスを付ける
--
-- >>> indexed "abc"
-- [(1,'a'),(2,'b'),(3,'c')]
-- >>> indexed ([] :: [Int])
-- []
indexed :: [b] -> [(Int, b)]
indexed = zip [1 ..]
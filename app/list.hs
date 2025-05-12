{--  リスト走査 --}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeGetElement :: [a] -> Int -> Maybe a
safeGetElement [] _ = Nothing
safeGetElement (x : xs) 0 = Just x
safeGetElement (x : xs) n
  | n < 0 = Nothing
  | otherwise = safeGetElement xs (n - 1)

findIndexFrom :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndexFrom i f s =
  case findIndex f (drop i s) of
    Just j -> Just (i + j)
    Nothing -> Nothing

elemIndexFrom :: (Eq a) => Int -> a -> [a] -> Maybe Int
elemIndexFrom i x = findIndexFrom i (== x)

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x : xs) = [(x, y) | y <- xs] ++ combinations xs

safeMaximum :: (Ord a) => [a] -> Maybe a
safeMaximum lst
  | null lst = Nothing
  | otherwise = Just $ maximum lst

findOnFst :: (t -> Bool) -> [(t, b)] -> Maybe (t, b)
findOnFst f = find (f . fst)

findOnSnd :: (Foldable t) => (b -> Bool) -> t (a, b) -> Maybe (a, b)
findOnSnd f = find (f . snd)

-- リストのコンビネーション
combinationList :: [[a]] -> [[a]]
combinationList [] = [[]]
combinationList (xs : xss) = [x : ys | x <- xs, ys <- combinationList xss]

-- 数字のパターンを重複を許して出す
combinationsWithRepetition :: Int -> Int -> Int -> [[Int]]
combinationsWithRepetition l r = go
  where
    go 0 = [[]]
    go k = [x : xs | x <- [l .. r], xs <- go (k - 1), x <= head (x : xs) || null xs]

-- 指定した数だけ抽出して組み合わせる
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations n as@(_ : xs)
  | n == 0 = [[]]
  | n == 1 = map pure as
  | n == l = pure as
  | n > l = []
  | otherwise = run (l - 1) (n - 1) as $ combinations (n - 1) xs
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

forAccumM :: (Monad m, Traversable t) => s -> t a -> (s -> a -> m (s, b)) -> m (s, t b)
forAccumM initial xs f = mapAccumM f initial xs

indexed :: [b] -> [(Int, b)]
indexed = zip [1 ..]
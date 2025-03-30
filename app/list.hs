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

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum lst
  | null lst = Nothing
  | otherwise = Just $ maximum lst
dfs :: Graph -> IS.IntSet -> Int -> IS.IntSet
dfs g seen v
  | IS.member v seen = seen
  | otherwise = L.foldl' (dfs g) seen' next_vs
  where
    next_vs = g IA.! v
    seen' = IS.insert v seen
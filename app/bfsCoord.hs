-- 二次元座標で、複数のスタート地点からbfsをする
bfsCoord :: (IArray UArray a) => UArray (Int, Int) a -> Int -> (UArray (Int, Int) a -> (Int, Int) -> Bool) -> S.Set (Int, Int) -> S.Set (Int, Int)
bfsCoord coord d f = inner 0 S.empty
  where
    bnds@(_, (h, w)) = bounds coord
    inner cnt visited news
      | cnt == d = visited1
      | S.null news = visited1
      | otherwise = inner (succ cnt) visited1 news1
      where
        visited1 = S.union visited news
        news1 =
          S.fromList
            [ dij
              | (i, j) <- S.elems news,
                dij <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)],
                S.notMember dij visited,
                inRange bnds dij,
                f coord dij
            ]
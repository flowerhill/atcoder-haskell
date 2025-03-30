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

-- startからgoalにいくやつ
type Point = (Int, Int)

type Grid = UArray (Int, Int) Char

bfs :: Grid -> Point -> Point -> Bool -> Maybe Int
bfs grid start goal isVertical = bfs' (Seq.singleton (start, 0, isVertical)) (S.singleton start)
  where
    boundsGrid = bounds grid
    valid point visited = inRange boundsGrid point && grid ! point /= '#' && point `S.notMember` visited

    bfs' :: Seq.Seq (Point, Int, Bool) -> S.Set Point -> Maybe Int
    bfs' q visited =
      case Seq.viewl q of
        Seq.EmptyL -> Nothing
        (pos@(r, c), dist, isVertical) Seq.:< rest ->
          if pos == goal
            then Just dist
            else
              let nextDirs = if isVertical then [(-1, 0), (1, 0)] else [(0, -1), (0, 1)]
                  nextPoints = [(r + dr, c + dc) | (dr, dc) <- nextDirs, let (nr, nc) = (r + dr, c + dc), valid (nr, nc) visited]
                  newQueue = rest Seq.>< Seq.fromList [(p, dist + 1, not isVertical) | p <- nextPoints]
                  newVisited = folr S.insert pos visited
               in bfs' newQueue newVisited
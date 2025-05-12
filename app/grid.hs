makeCharGrid :: (IArray UArray e, Ix i, Foldable t) => (i, i) -> t [e] -> UArray i e
makeCharGrid (l, h) lst = listArray @UArray (l, h) $ concat lst

printGrid :: (Applicative f, IArray a i, Ix v) => ([i] -> f b) -> a (v, Int) i -> f ()
printGrid f grid = traverse_ f $ chunksOf w (elems grid)
  where
    ((_, w1), (_, w2)) = bounds grid
    w = w2 + 1 - w1

printCharGrid :: (IArray a Char, Ix v) => a (v, Int) Char -> IO ()
printCharGrid = printGrid putStrLn

printIntGrid :: (Show e, IArray a e, Ix v) => a (v, Int) e -> IO ()
printIntGrid = printGrid printList

-- bfs
-- 二次元座標で、複数のスタート地点からbfsをする
bfsGrid :: (IArray UArray a) => UArray (Int, Int) a -> Int -> (UArray (Int, Int) a -> (Int, Int) -> Bool) -> S.Set (Int, Int) -> S.Set (Int, Int)
bfsGrid coord d f = go 0 S.empty
  where
    bnds@(_, (h, w)) = bounds coord
    go cnt visited news
      | cnt == d = visited1
      | S.null news = visited1
      | otherwise = go (succ cnt) visited1 news1
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

-- startからgoalを指定
bfsGrid2 :: UArray (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool -> Maybe Int
bfsGrid2 grid start goal isVertical = go (Seq.singleton (start, 0, isVertical)) (S.singleton start)
  where
    boundsGrid = bounds grid
    valid point visited = inRange boundsGrid point && grid ! point /= '#' && point `S.notMember` visited
    go :: Seq.Seq ((Int, Int), Int, Bool) -> S.Set (Int, Int) -> Maybe Int
    go q visited =
      case Seq.viewl q of
        Seq.EmptyL -> Nothing
        (pos@(r, c), dist, isVertical) Seq.:< rest ->
          if pos == goal
            then Just dist
            else
              let nextDirs = if isVertical then [(-1, 0), (1, 0)] else [(0, -1), (0, 1)]
                  nextPoints = [(r + dr, c + dc) | (dr, dc) <- nextDirs, let (nr, nc) = (r + dr, c + dc), valid (nr, nc) visited]
                  newQueue = rest Seq.>< Seq.fromList [(p, dist + 1, not isVertical) | p <- nextPoints]
                  newVisited = S.insert pos visited
               in go newQueue newVisited

-- 座標用dfs
-- https://atcoder.jp/contests/abc335/tasks/abc335_d

type Coord = (Int, Int)

type Path = [Coord]

type Bounds = (Coord, Coord)

-- 座標を外からまわっていく
dfs :: Bounds -> Path -> S.Set Coord -> Coord -> Coord -> Coord -> [Coord]
dfs b path visited d@(di, dj) v@(vi, vj) goal
  | v == goal = reverse path
  | otherwise =
      let v'@(vi', vj') = (vi + di, vj + dj)
          (u, d') =
            if S.member v' visited || not (inRange b v')
              then ((vi + dj, vj - di), (dj, -di))
              else (v', d)
       in dfs b (v : path) (S.insert v visited) d' u goal

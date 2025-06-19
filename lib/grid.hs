type Point = (Int, Int)

type Bounds = (Point, Point)

type Grid = UArray Point Char

mv@[left, right, up, down] = [(0, -1), (0, 1), (-1, 0), (1, 0)] :: [(Int, Int)]

buildGrid :: Bounds -> [Char] -> UArray Point Char
buildGrid = listArray

-- mutable版 IOUArrayを使用
buildIOGrid :: Bounds -> [Char] -> IO (IOUArray Point Char)
buildIOGrid = newListArray

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

dfsCntIORef :: (Enum a, Foldable t1, Ord t2, Num t3, Eq t3) => t3 -> (t2 -> t1 t2) -> MV.IORef a -> S.Set t2 -> t3 -> t2 -> IO ()
dfsCntIORef k f cnt visited i v
  -- countを+1する。ここはIORefを使う
  | k == i = modifyIORef' cnt succ
  | otherwise = do
      for_ (f v) $ \u -> do
        when (S.notMember u visited) $ do
          dfsCntIORef k f cnt (S.insert u visited) (i + 1) u

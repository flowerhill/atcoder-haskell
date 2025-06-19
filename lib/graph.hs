-- 深さ優先探索（グラフ用）
type Vertex = Int

type Weight = Int

type Bounds = (Int, Int)

-- 重みなし
type Edge = Vertex

type Graph = Array Vertex [Edge]

-- 重み付き
type EdgeW = (Vertex, Weight)

type GraphW = Array Vertex [EdgeW]

-- 重みなしグラフ構築(有向グラフ)
buildG :: Bounds -> [[Int]] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v)]) edges

-- 重みなしグラフ構築(無向グラフ)
buildG2 :: Bounds -> [[Int]] -> Graph
buildG2 bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v), (v, u)]) edges

-- 重みつきグラフ構築(有向グラフ)
buildGW :: Bounds -> [[Int]] -> GraphW
buildGW (start, end) edges = accumArray (flip (:)) [] (start, end) edges'
  where
    edges' = concatMap (\[u, v, w] -> [((u, v), w)]) edges

-- 重みつきグラフ構築(無向グラフ)
buildGW2 :: Bounds -> [[Int]] -> GraphW
buildGW2 (start, end) edges = accumArray (flip (:)) [] (start, end) edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w)), (v, (u, w))]) edges

dfs :: Graph -> IS.IntSet -> Int -> IS.IntSet
dfs g seen v
  | IS.member v seen = seen
  | otherwise = foldl' (dfs g) seen' next_vs
  where
    next_vs = g ! v
    seen' = IS.insert v seen

-- startとgoalが決まっている場合のdfs
-- startからgoalまでのすべての経路を返す
dfs2 :: (Eq p) => (p -> [p]) -> p -> p -> [[p]]
dfs2 next start goal = go [start]
  where
    go [] = []
    go path@(head : tail)
      | head == goal = return path
      | otherwise = do
          guard $ head `notElem` tail
          u <- next head
          go (u : path)

{-- bfs --}
-- BFSを実行し、始点からの距離をUArrayで返す
bfs :: (Ix i, Foldable t) => (i -> t i) -> Int -> (i, i) -> [i] -> UArray i Int
bfs nextStates init (l, h) starts = runSTUArray $ do
  -- 距離配列を初期化
  dist <- newArray (l, h) init
  -- 探索キュー
  queue <- newSTRef (Seq.fromList starts)
  -- 始点の距離を0に設定
  forM_ starts $ \start ->
    writeArray dist start 0

  -- メインのBFSループ
  fix $ \loop -> do
    q <- readSTRef queue
    if Seq.null q
      then return ()
      else do
        let (v Seq.:< rest) = Seq.viewl q
        writeSTRef queue rest

        -- 現在の頂点の距離を取得
        currentDist <- readArray dist v
        -- 隣接頂点を処理
        forM_ (nextStates v) $ \u -> do
          uDist <- readArray dist u
          when (uDist == init) $ do
            writeArray dist u (currentDist + 1)
            modifySTRef' queue (Seq.>< Seq.singleton u)
        loop

  return dist

-- immutable版
bfs :: Int -> Graph -> Seq.Seq (Int, Int) -> M.Map Int Int -> M.Map Int Int
bfs n g queue visited
  | null queue = visited
bfs n g ((q, l) Seq.:<| queue) visited
  | M.member q visited = bfs n g queue visited
  | otherwise =
      let visited' = M.insert q l visited
          next = g ! q
          queue' = queue Seq.>< Seq.fromList (map (,succ l) next)
       in bfs n g queue' visited'
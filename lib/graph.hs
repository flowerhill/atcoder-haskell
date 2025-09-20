-- 重みなしグラフ構築(有向グラフ)
buildG :: (Int, Int) -> [[Int]] -> Array Int [Int]
buildG bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v)]) edges

-- 重みなしグラフ構築(無向グラフ)
buildG2 :: (Int, Int) -> [[Int]] -> Array Int [Int]
buildG2 bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v), (v, u)]) edges

-- 重みつきグラフ構築(有向グラフ)
buildGW :: (Int, Int) -> [[Int]] -> Array Int [(Int, Int)] -- edgeを(番号, 重み) で表現
buildGW (start, end) edges = accumArray (flip (:)) [] (start, end) edges'
  where
    edges' = concatMap (\[u, v, w] -> [((u, v), w)]) edges

-- 重みつきグラフ構築(無向グラフ)
buildGW2 :: (Int, Int) -> [[Int]] -> Array Int [(Int, Int)] -- edgeを(番号, 重み) で表現
buildGW2 (start, end) edges = accumArray (flip (:)) [] (start, end) edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w)), (v, (u, w))]) edges

-- BFS単始点版（immutable）
bfsSingleSource :: (Int -> [Int]) -> Int -> IS.IntSet
bfsSingleSource getNext start = go IS.empty [start]
  where
    go visited [] = visited
    go visited queue =
      let nodes' =
            [ node | curr <- queue, node <- getNext curr, not (IS.member node visited)
            ]
          visited' = foldl (flip IS.insert) visited queue
       in if null nodes'
            then visited'
            else go visited' nodes'

-- BFS複数始点版（immutable）
bfs :: (Int -> [Int]) -> IS.IntSet -> [Int] -> IS.IntSet
bfs getNext visited [] = visited
bfs getNext visited queue =
  let nodes' =
        [ node | curr <- queue, node <- getNext curr, not (IS.member node visited)
        ]
      visited' = foldl (flip IS.insert) visited nodes'
   in if null nodes'
        then visited
        else bfs getNext visited' nodes'

-- BFS単始点版（mutable）
-- 訪問済みをTrue, 未訪問をFalseで返す
bfsSingleSourceSTUArray :: Int -> (Int -> [Int]) -> Int -> UArray Int Bool
bfsSingleSourceSTUArray n getNext start = runSTUArray $ do
  visited <- newArray (1, n) False
  queueRef <- newSTRef [start]

  let loop = do
        queue <- readSTRef queueRef
        case queue of
          [] -> return visited
          currentLevel -> do
            writeSTRef queueRef []
            nextLevel <- foldM processNode [] currentLevel
            if null nextLevel
              then return visited
              else do
                writeSTRef queueRef nextLevel
                loop

      processNode acc curr = do
        isVisited <- readArray visited curr
        if isVisited
          then return acc
          else do
            writeArray visited curr True
            let neighbors = getNext curr
            return (neighbors ++ acc)
  loop

-- BFS複数始点版（mutable）
-- 訪問済みをTrue, 未訪問をFalseで返す
bfsRunSTUArray :: Int -> (Int -> [Int]) -> [Int] -> UArray Int Bool
bfsRunSTUArray n getNext initNodes = runSTUArray $ do
  visited <- newArray (1, n) False
  queueRef <- newSTRef initNodes

  let loop = do
        queue <- readSTRef queueRef
        case queue of
          [] -> return visited
          currentLevel -> do
            writeSTRef queueRef []
            nextLevel <- foldM processNode [] currentLevel
            if null nextLevel
              then return visited
              else do
                writeSTRef queueRef nextLevel
                loop

      processNode acc curr = do
        isVisited <- readArray visited curr
        if isVisited
          then return acc
          else do
            writeArray visited curr True
            let neighbors = getNext curr
            return (neighbors ++ acc)
  loop

-- BFS最短経路単始点版（mutable）
bfsShortestPathSTUArray :: Int -> (Int -> [Int]) -> Int -> UArray Int Int
bfsShortestPathSTUArray n getNext start = runSTUArray $ do
  distance <- newArray (1, n) (-1) -- -1 = 未訪問
  queueRef <- newSTRef [start]
  writeArray distance start 0 -- 開始点の距離は0
  let loop = do
        queue <- readSTRef queueRef
        case queue of
          [] -> return distance
          currentLevel -> do
            writeSTRef queueRef []
            nextLevel <- foldM processNode [] currentLevel
            if null nextLevel
              then return distance
              else do
                writeSTRef queueRef nextLevel
                loop

      processNode acc curr = do
        currDist <- readArray distance curr
        if currDist == -1 -- 未訪問なら無視
          then return acc
          else do
            neighbors <- return $ getNext curr
            newNodes <- foldM (checkNeighbor currDist) acc neighbors
            return newNodes

      checkNeighbor currDist acc neighbor = do
        neighborDist <- readArray distance neighbor
        if neighborDist == -1 -- 未訪問の場合のみ
          then do
            writeArray distance neighbor (currDist + 1)
            return (neighbor : acc)
          else return acc
  loop

-- BFS最短経路複数始点版（mutable）
bfsMultiSourceShortestPath :: Int -> (Int -> [Int]) -> [Int] -> UArray Int Int
bfsMultiSourceShortestPath n getNext starts = runSTUArray $ do
  distance <- newArray (1, n) (-1)
  queueRef <- newSTRef starts

  -- 全ての開始点を距離0で初期化
  mapM_ (\start -> writeArray distance start 0) starts

  let loop = do
        queue <- readSTRef queueRef
        case queue of
          [] -> return distance
          currentLevel -> do
            writeSTRef queueRef []
            nextLevel <- foldM processNode [] currentLevel
            if null nextLevel
              then return distance
              else do
                writeSTRef queueRef nextLevel
                loop

      processNode acc curr = do
        currDist <- readArray distance curr
        neighbors <- return $ getNext curr
        foldM (checkNeighbor currDist) acc neighbors

      checkNeighbor currDist acc neighbor = do
        neighborDist <- readArray distance neighbor
        if neighborDist == -1
          then do
            writeArray distance neighbor (currDist + 1)
            return (neighbor : acc)
          else return acc
  loop

-- DFS単始点版(immutable)
dfsSingleSource :: (Int -> [Int]) -> IS.IntSet -> Int -> IS.IntSet
dfsSingleSource getNext visited start
  | IS.member start visited = visited
  | otherwise =
      let visited' = IS.insert start visited
          neighbors = getNext start
       in foldl' (dfsSingleSource getNext) visited' neighbors

-- DFS複数始点版(immutable)
dfs :: (Int -> [Int]) -> IS.IntSet -> [Int] -> IS.IntSet
dfs getNext visited [] = visited
dfs getNext visited (curr : rest)
  | IS.member curr visited = dfs getNext visited rest
  | otherwise =
      let visited' = IS.insert curr visited
          neighbors = getNext curr
       in dfs getNext visited' (neighbors ++ rest)

-- DFS単始点版(mutable)
-- 訪問済みをTrue, 未訪問をFalseで返す
dfsSingleSourceSTUArray :: Int -> (Int -> [Int]) -> Int -> UArray Int Bool
dfsSingleSourceSTUArray n getNext start = runSTUArray $ do
  visited <- newArray (1, n) False
  stackRef <- newSTRef [start]

  let loop = do
        stack <- readSTRef stackRef
        case stack of
          [] -> return visited
          (curr : rest) -> do
            writeSTRef stackRef rest
            isVisited <- readArray visited curr
            if isVisited
              then loop
              else do
                writeArray visited curr True
                let neighbors = getNext curr
                modifySTRef stackRef (neighbors ++)
                loop
  loop

-- 複数始点(mutable)
-- 訪問済みをTrue, 未訪問をFalseで返す
dfsRunSTUArray :: Int -> (Int -> [Int]) -> [Int] -> UArray Int Bool
dfsRunSTUArray n getNext initNodes = runSTUArray $ do
  visited <- newArray (1, n) False
  stackRef <- newSTRef initNodes

  let loop = do
        stack <- readSTRef stackRef
        case stack of
          [] -> return visited
          (curr : rest) -> do
            writeSTRef stackRef rest
            isVisited <- readArray visited curr
            if isVisited
              then loop
              else do
                writeArray visited curr True
                let neighbors = getNext curr
                modifySTRef' stackRef (neighbors ++)
                loop
  loop
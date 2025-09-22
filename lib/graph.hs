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

-- 連結成分の個数（汎用版）
-- DFS版：遷移関数を受け取る
countComponentsDFS :: Int -> (Int -> [Int]) -> Int
countComponentsDFS n getNext = runST $ do
  visited <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  count <- newSTRef 0

  let dfs v = do
        seen <- readArray visited v
        unless seen $ do
          writeArray visited v True
          mapM_ dfs (getNext v)

  forM_ [1 .. n] $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      modifySTRef' count (+ 1)
      dfs v

  readSTRef count

-- BFS版：遷移関数を受け取る
countComponentsBFS :: Int -> (Int -> [Int]) -> Int
countComponentsBFS n getNext = runST $ do
  visited <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  count <- newSTRef 0

  let bfs startV = do
        queue <- newSTRef [startV]
        writeArray visited startV True

        let loop = do
              q <- readSTRef queue
              case q of
                [] -> return ()
                (v : rest) -> do
                  writeSTRef queue rest
                  neighbors <-
                    filterM
                      ( \u -> do
                          seen <- readArray visited u
                          return (not seen)
                      )
                      (getNext v)
                  mapM_ (\u -> writeArray visited u True) neighbors
                  modifySTRef' queue (++ neighbors)
                  loop
        loop

  forM_ [1 .. n] $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      modifySTRef' count (+ 1)
      bfs v

  readSTRef count

-- =============================================================================
-- 連結成分の詳細（汎用版）
-- =============================================================================

-- 各成分の頂点リスト（DFS）
getComponentsDFS :: Int -> (Int -> [Int]) -> [[Int]]
getComponentsDFS n getNext = runST $ do
  visited <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  components <- newSTRef []

  let dfs v acc = do
        seen <- readArray visited v
        if not seen
          then do
            writeArray visited v True
            foldM (flip dfs) (v : acc) (getNext v)
          else return acc

  forM_ [1 .. n] $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      component <- dfs v []
      unless (null component) $
        modifySTRef' components (component :)

  readSTRef components

-- 各成分の頂点リスト（BFS）
getComponentsBFS :: Int -> (Int -> [Int]) -> [[Int]]
getComponentsBFS n getNext = runST $ do
  visited <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  components <- newSTRef []

  let bfs startV = do
        queue <- newSTRef [startV]
        component <- newSTRef []
        writeArray visited startV True
        modifySTRef' component (startV :)

        let loop = do
              q <- readSTRef queue
              case q of
                [] -> return ()
                (v : rest) -> do
                  writeSTRef queue rest
                  neighbors <-
                    filterM
                      ( \u -> do
                          seen <- readArray visited u
                          return (not seen)
                      )
                      (getNext v)
                  mapM_
                    ( \u -> do
                        writeArray visited u True
                        modifySTRef' component (u :)
                    )
                    neighbors
                  modifySTRef' queue (++ neighbors)
                  loop

        loop
        readSTRef component

  forM_ [1 .. n] $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      component <- bfs v
      modifySTRef' components (component :)

  readSTRef components

-- 各成分のサイズ
getComponentSizes :: Int -> (Int -> [Int]) -> [Int]
getComponentSizes n getNext = map length (getComponentsDFS n getNext)

-- 最大成分のサイズ
getLargestComponent :: Int -> (Int -> [Int]) -> Int
getLargestComponent n getNext =
  let sizes = getComponentSizes n getNext
   in if null sizes then 0 else maximum sizes

-- =============================================================================
-- 特殊関数（汎用版）
-- =============================================================================

getComponentSize :: Int -> (Int -> [Int]) -> Int -> Int
getComponentSize n getNext v = runST $ do
  visited <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  size <- newSTRef 0

  let dfs u = do
        seen <- readArray visited u
        unless seen $ do
          writeArray visited u True
          modifySTRef' size (+ 1)
          mapM_ dfs (getNext u)

  if v >= 1 && v <= n
    then do
      dfs v
      readSTRef size
    else return 0

-- 同じ成分かチェック
isConnected :: Int -> (Int -> [Int]) -> Int -> Int -> Bool
isConnected n getNext u v = runST $ do
  visited <- newArray (1, n) False :: ST s (STUArray s Int Bool)
  found <- newSTRef False

  let dfs curr = do
        when (curr == v) $ writeSTRef found True
        seen <- readArray visited curr
        foundFlag <- readSTRef found
        when (not seen && not foundFlag) $ do
          writeArray visited curr True
          mapM_ dfs (getNext curr)

  if u >= 1 && u <= n && v >= 1 && v <= n
    then do
      dfs u
      readSTRef found
    else return False

-- =============================================================================
-- Array版（便利関数）
-- =============================================================================

-- Array版：連結成分数（DFS）
countComponentsArrayDFS :: Array Int [Int] -> Int
countComponentsArrayDFS graph =
  let (start, end) = bounds graph
      getNext v = graph ! v
   in countComponentsDFS (end - start + 1) getNext

-- Array版：連結成分数（BFS）
countComponentsArrayBFS :: Array Int [Int] -> Int
countComponentsArrayBFS graph =
  let (start, end) = bounds graph
      getNext v = graph ! v
   in countComponentsBFS (end - start + 1) getNext

-- Array版：連結成分リスト
getComponentsArray :: Array Int [Int] -> [[Int]]
getComponentsArray graph =
  let (start, end) = bounds graph
      getNext v = graph ! v
   in getComponentsDFS (end - start + 1) getNext

-- =============================================================================
-- グリッド用応用例
-- =============================================================================

-- グリッド用の遷移関数例
gridGetNext :: UArray (Int, Int) Char -> Int -> Int -> (Int, Int) -> [(Int, Int)]
gridGetNext grid h w (r, c) =
  let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
      neighbors = map (bimap (r +) (c +)) directions
      valid (nr, nc) = nr >= 1 && nr <= h && nc >= 1 && nc <= w && grid ! (nr, nc) == '#'
   in filter valid neighbors

-- グリッド島カウント例（座標を1次元に変換）
countGridIslands :: Int -> Int -> UArray (Int, Int) Char -> Int
countGridIslands h w grid =
  let coordToIndex (r, c) = (r - 1) * w + c
      indexToCoord idx = let (r', c') = divMod (idx - 1) w in (r' + 1, c' + 1)

      getNext idx =
        let pos = indexToCoord idx
         in if grid ! pos == '#'
              then map coordToIndex (gridGetNext grid h w pos)
              else []

      isLand idx = let pos = indexToCoord idx in grid ! pos == '#'
      validIndices = filter isLand [1 .. h * w]
   in length $
        filter (not . null) $
          getComponentsDFS (h * w) getNext

{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.IntSet as IS
import Data.Ix
import Data.List (foldl')
import Data.STRef

-- =============================================================================
-- グラフ構築関数（汎用化版）
-- =============================================================================

-- 重みなしグラフ構築(有向グラフ)
buildG :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [a]
buildG bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v)]) edges

-- 重みなしグラフ構築(無向グラフ)
buildG2 :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [a]
buildG2 bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v), (v, u)]) edges

-- 重みつきグラフ構築(有向グラフ)
buildGW :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [(a, a)]
buildGW bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w))]) edges

-- 重みつきグラフ構築(無向グラフ)
buildGW2 :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [(a, a)]
buildGW2 bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w)), (v, (u, w))]) edges

-- =============================================================================
-- BFS系関数（汎用化版）
-- =============================================================================

-- BFS単始点版（immutable）
bfsSingleSource :: forall a. (Ix a) => (a -> [a]) -> a -> IS.IntSet
bfsSingleSource getNext start = go IS.empty [start]
  where
    go visited [] = visited
    go visited queue =
      let nodes' = [node | curr <- queue, node <- getNext curr, not (IS.member (fromEnum node) visited)]
          visited' = foldl (flip IS.insert) visited (map fromEnum queue)
       in if null nodes'
            then visited'
            else go visited' nodes'

-- BFS複数始点版（immutable）
bfs :: forall a. (Ix a) => (a -> [a]) -> IS.IntSet -> [a] -> IS.IntSet
bfs getNext visited [] = visited
bfs getNext visited queue =
  let nodes' = [node | curr <- queue, node <- getNext curr, not (IS.member (fromEnum node) visited)]
      visited' = foldl (flip IS.insert) visited (map fromEnum nodes')
   in if null nodes'
        then visited
        else bfs getNext visited' nodes'

-- BFS単始点版（mutable）
bfsSingleSourceSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> UArray a Bool
bfsSingleSourceSTUArray bounds getNext start = runSTUArray $ do
  visited <- newArray bounds False
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
            let neighbors = filter (inRange bounds) (getNext curr)
            return (neighbors ++ acc)
  loop

-- BFS複数始点版（mutable）
bfsRunSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> UArray a Bool
bfsRunSTUArray bounds getNext initNodes = runSTUArray $ do
  visited <- newArray bounds False
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
            let neighbors = filter (inRange bounds) (getNext curr)
            return (neighbors ++ acc)
  loop

-- BFS最短経路単始点版（mutable）
bfsShortestPathSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> UArray a Int
bfsShortestPathSTUArray bounds getNext start = runSTUArray $ do
  distance <- newArray bounds (-1)
  queueRef <- newSTRef [start]
  writeArray distance start 0

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
        if currDist == -1
          then return acc
          else do
            let neighbors = filter (inRange bounds) (getNext curr)
            newNodes <- foldM (checkNeighbor currDist) acc neighbors
            return newNodes

      checkNeighbor currDist acc neighbor = do
        neighborDist <- readArray distance neighbor
        if neighborDist == -1
          then do
            writeArray distance neighbor (currDist + 1)
            return (neighbor : acc)
          else return acc
  loop

-- BFS最短経路複数始点版（mutable）
bfsMultiSourceShortestPath :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> UArray a Int
bfsMultiSourceShortestPath bounds getNext starts = runSTUArray $ do
  distance <- newArray bounds (-1)
  queueRef <- newSTRef starts

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
        let neighbors = filter (inRange bounds) (getNext curr)
        foldM (checkNeighbor currDist) acc neighbors

      checkNeighbor currDist acc neighbor = do
        neighborDist <- readArray distance neighbor
        if neighborDist == -1
          then do
            writeArray distance neighbor (currDist + 1)
            return (neighbor : acc)
          else return acc
  loop

-- =============================================================================
-- DFS系関数（汎用化版）
-- =============================================================================

-- DFS単始点版(immutable)
dfsSingleSource :: forall a. (Ix a) => (a -> [a]) -> IS.IntSet -> a -> IS.IntSet
dfsSingleSource getNext visited start
  | IS.member (fromEnum start) visited = visited
  | otherwise =
      let visited' = IS.insert (fromEnum start) visited
          neighbors = getNext start
       in foldl' (dfsSingleSource getNext) visited' neighbors

-- DFS複数始点版(immutable)
dfs :: forall a. (Ix a) => (a -> [a]) -> IS.IntSet -> [a] -> IS.IntSet
dfs getNext visited [] = visited
dfs getNext visited (curr : rest)
  | IS.member (fromEnum curr) visited = dfs getNext visited rest
  | otherwise =
      let visited' = IS.insert (fromEnum curr) visited
          neighbors = getNext curr
       in dfs getNext visited' (neighbors ++ rest)

-- DFS単始点版(mutable)
dfsSingleSourceSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> UArray a Bool
dfsSingleSourceSTUArray bounds getNext start = runSTUArray $ do
  visited <- newArray bounds False
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
                let neighbors = filter (inRange bounds) (getNext curr)
                modifySTRef stackRef (neighbors ++)
                loop
  loop

-- 複数始点(mutable)
dfsRunSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> UArray a Bool
dfsRunSTUArray bounds getNext initNodes = runSTUArray $ do
  visited <- newArray bounds False
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
                let neighbors = filter (inRange bounds) (getNext curr)
                modifySTRef' stackRef (neighbors ++)
                loop
  loop

-- 各ノードへの経路を返す（未訪問は空リスト）
data PathNode a = PathNode a [a]

dfsRunSTUArrayWithPath :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> UArray a [a]
dfsRunSTUArrayWithPath bounds getNext initNodes = runSTUArray $ do
  visited <- newArray bounds False
  paths <- newArray bounds []
  stackRef <- newSTRef $ map (\x -> PathNode x [x]) initNodes

  let loop = do
        stack <- readSTRef stackRef
        case stack of
          [] -> return paths
          (PathNode curr currentPath : rest) -> do
            writeSTRef stackRef rest
            isVisited <- readArray visited curr
            if isVisited
              then loop
              else do
                writeArray visited curr True
                writeArray paths curr currentPath
                let neighbors = filter (inRange bounds) (getNext curr)
                let neighborPaths = map (\next -> PathNode next (currentPath ++ [next])) neighbors
                modifySTRef' stackRef (neighborPaths ++)
                loop
  loop

-- =============================================================================
-- 連結成分関数（汎用化版）
-- =============================================================================

-- 連結成分の個数（DFS版）
countComponentsDFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> Int
countComponentsDFS bounds getNext = runST $ do
  visited <- newArray bounds False :: ST s (STUArray s a Bool)
  count <- newSTRef 0

  let dfs v = do
        seen <- readArray visited v
        unless seen $ do
          writeArray visited v True
          mapM_ dfs (filter (inRange bounds) (getNext v))

  forM_ (range bounds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      modifySTRef' count (+ 1)
      dfs v

  readSTRef count

-- 連結成分の個数（BFS版）
countComponentsBFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> Int
countComponentsBFS bounds getNext = runST $ do
  visited <- newArray bounds False :: ST s (STUArray s a Bool)
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
                      (filter (inRange bounds) (getNext v))
                  mapM_ (\u -> writeArray visited u True) neighbors
                  modifySTRef' queue (++ neighbors)
                  loop
        loop

  forM_ (range bounds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      modifySTRef' count (+ 1)
      bfs v

is
  readSTRef
  count

-- 各成分の頂点リスト（DFS）
getComponentsDFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [[a]]
getComponentsDFS bounds getNext = runST $ do
  visited <- newArray bounds False :: ST s (STUArray s a Bool)
  components <- newSTRef []

  let dfs v acc = do
        seen <- readArray visited v
        if not seen
          then do
            writeArray visited v True
            foldM (flip dfs) (v : acc) (filter (inRange bounds) (getNext v))
          else return acc

  forM_ (range bounds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      component <- dfs v []
      unless (null component) $
        modifySTRef' components (component :)

  readSTRef components

-- 各成分の頂点リスト（BFS）
getComponentsBFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [[a]]
getComponentsBFS bounds getNext = runST $ do
  visited <- newArray bounds False :: ST s (STUArray s a Bool)
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
                      (filter (inRange bounds) (getNext v))
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

  forM_ (range bounds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      component <- bfs v
      modifySTRef' components (component :)

  readSTRef components

-- 各成分のサイズ
getComponentSizes :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [Int]
getComponentSizes bounds getNext = map length (getComponentsDFS bounds getNext)

-- 最大成分のサイズ
getLargestComponent :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> Int
getLargestComponent bounds getNext =
  let sizes = getComponentSizes bounds getNext
   in if null sizes then 0 else maximum sizes

-- =============================================================================
-- 特殊関数（汎用化版）
-- =============================================================================

-- 指定ノードの成分サイズ
getComponentSize :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> Int
getComponentSize bounds getNext v = runST $ do
  visited <- newArray bounds False :: ST s (STUArray s a Bool)
  size <- newSTRef 0

  let dfs u = do
        seen <- readArray visited u
        unless seen $ do
          writeArray visited u True
          modifySTRef' size (+ 1)
          mapM_ dfs (filter (inRange bounds) (getNext u))

  if inRange bounds v
    then do
      dfs v
      readSTRef size
    else return 0

-- 同じ成分かチェック
isConnected :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> a -> Bool
isConnected bounds getNext u v
  | not (inRange bounds u) || not (inRange bounds v) = False
  | u == v = True
  | otherwise = runST $ do
      visited <- newArray bounds False :: ST s (STUArray s a Bool)
      found <- newSTRef False

      let dfs curr = do
            when (curr == v) $ writeSTRef found True
            seen <- readArray visited curr
            foundFlag <- readSTRef found
            when (not seen && not foundFlag) $ do
              writeArray visited curr True
              mapM_ dfs (filter (inRange bounds) (getNext curr))

      dfs u
      readSTRef found

-- =============================================================================
-- Array版（便利関数）
-- =============================================================================

-- Array版：連結成分数（DFS）
countComponentsArrayDFS :: forall a. (Ix a) => Array a [a] -> Int
countComponentsArrayDFS graph =
  let bounds' = bounds graph
      getNext v = graph ! v
   in countComponentsDFS bounds' getNext

-- Array版：連結成分数（BFS）
countComponentsArrayBFS :: forall a. (Ix a) => Array a [a] -> Int
countComponentsArrayBFS graph =
  let bounds' = bounds graph
      getNext v = graph ! v
   in countComponentsBFS bounds' getNext

-- Array版：連結成分リスト
getComponentsArray :: forall a. (Ix a) => Array a [a] -> [[a]]
getComponentsArray graph =
  let bounds' = bounds graph
      getNext v = graph ! v
   in getComponentsDFS bounds' getNext

-- =============================================================================
-- グリッド系（汎用化版）
-- =============================================================================

-- gridの構築
buildGrid :: (IArray UArray e, Ix i, Foldable t) => (i, i) -> t [e] -> UArray i e
buildGrid bnds lst = listArray @UArray bnds $ concat lst

-- グリッド上の隣接座標を取得する関数（4方向版）
getNextGrid4 :: UArray (Int, Int) Char -> Char -> (Int, Int) -> [(Int, Int)]
getNextGrid4 grid wallChar (r, c) =
  let bounds' = bounds grid
      directions = [(-1, 0), (1, 0), (0, -1), (0, 1)] -- 上下左右
      neighbors = [(r + dr, c + dc) | (dr, dc) <- directions]
      validNeighbors =
        [ (nr, nc)
          | (nr, nc) <- neighbors,
            inRange bounds' (nr, nc),
            grid ! (nr, nc) /= wallChar -- 塀以外
        ]
   in validNeighbors

-- グリッド上の隣接座標を取得する関数（8方向版）
getNextGrid8 :: UArray (Int, Int) Char -> Char -> (Int, Int) -> [(Int, Int)]
getNextGrid8 grid wallChar (r, c) =
  let bounds' = bounds grid
      directions =
        [ (-1, -1),
          (-1, 0),
          (-1, 1), -- 左上、上、右上
          (0, -1),
          (0, 1), -- 左、右
          (1, -1),
          (1, 0),
          (1, 1) -- 左下、下、右下
        ]
      neighbors = [(r + dr, c + dc) | (dr, dc) <- directions]
      validNeighbors =
        [ (nr, nc) | (nr, nc) <- neighbors, inRange bounds' (nr, nc), grid ! (nr, nc) /= wallChar -- 境界チェック追加
        -- 塀以外
        ]
   in validNeighbors

-- 斜め方向に走査
diag :: (IArray UArray a) => UArray (Int, Int) a -> [[a]]
diag arr =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
      coords =
        [ [ (i, j) | i <- [minRow .. maxRow], j <- [minCol .. maxCol], i - j == sum
          ]
          | sum <- [minRow - maxCol .. maxRow - minCol]
        ]
   in [[arr ! c | c <- cc] | cc <- coords]

-- 逆斜め方向に走査
revDiag :: (IArray UArray a) => UArray (Int, Int) a -> [[a]]
revDiag arr =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
      coords =
        [ [ (i, j) | i <- [minRow .. maxRow], j <- [maxCol, maxCol - 1 .. minCol], i + j == sum
          ]
          | sum <- [minRow + minCol .. maxRow + maxCol]
        ]
   in [[arr ! c | c <- cc] | cc <- coords]

printIntGrid :: (Show e, IArray a e, Ix v) => a (v, Int) e -> IO ()
printIntGrid grid = traverse_ (putStrLn . unwords . map show) $ chunksOf w (elems grid)
  where
    ((_, w1), (_, w2)) = bounds grid
    w = w2 + 1 - w1

mvChar :: Char -> (Int, Int) -> (Int, Int)
mvChar 'L' (i, j) = (i, j - 1)
mvChar 'R' (i, j) = (i, j + 1)
mvChar 'U' (i, j) = (i - 1, j)
mvChar 'D' (i, j) = (i + 1, j)
mvChar _ pos = pos

mvList@[left, right, up, down] = [(0, -1), (0, 1), (-1, 0), (1, 0)] :: [(Int, Int)]

-- グリッド島カウント例（getNext関数を動的に受け取る版）
countGridIslands :: forall a. (Ix a) => UArray a Char -> (a -> [a]) -> Int
countGridIslands grid getNext =
  let bounds' = bounds grid
   in countComponentsDFS bounds' getNext
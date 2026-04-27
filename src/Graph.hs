{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Graph where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable (traverse_)
import Data.Foldable.Extra (toList)
import Data.List (foldl')
import Data.List.Extra (chunksOf)
import Data.STRef
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- =============================================================================
-- グラフ構築関数（汎用化版）
-- =============================================================================

-- | 重みなしグラフ構築（有向グラフ）
--
-- >>> buildG (1,3) [[1,2],[2,3]] ! 1
-- [2]
-- >>> buildG (1,3) [[1,2],[2,3]] ! 3
-- []
buildG :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [a]
buildG bnds edges = accumArray (flip (:)) [] bnds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v)]) edges

-- | 重みなしグラフ構築（無向グラフ）
--
-- >>> buildG2 (1,3) [[1,2],[2,3]] ! 2
-- [3,1]
buildG2 :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [a]
buildG2 bnds edges = accumArray (flip (:)) [] bnds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v), (v, u)]) edges

-- | 重みつきグラフ構築（有向グラフ）
--
-- >>> buildGW (1,3) [[1,2,5],[2,3,3]] ! 1
-- [(2,5)]
buildGW :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [(a, a)]
buildGW bnds edges = accumArray (flip (:)) [] bnds edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w))]) edges

-- | 重みつきグラフ構築（無向グラフ）
--
-- >>> buildGW2 (1,3) [[1,2,5],[2,3,3]] ! 2
-- [(3,3),(1,5)]
buildGW2 :: forall a. (Ix a) => (a, a) -> [[a]] -> Array a [(a, a)]
buildGW2 bnds edges = accumArray (flip (:)) [] bnds edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w)), (v, (u, w))]) edges

-- =============================================================================
-- BFS系関数（Data.Sequence版）
-- =============================================================================

-- | BFS単始点版（immutable, Data.Sequence使用）
--
-- >>> import qualified Data.Set as S
-- >>> S.toList $ bfsSingleSource (\x -> case x of 1 -> [2,3]; 2 -> [1]; 3 -> [1]; _ -> []) 1
-- [1,2,3]
bfsSingleSource :: forall a. (Ix a, Ord a) => (a -> [a]) -> a -> S.Set a
bfsSingleSource getNext start = go S.empty (Seq.singleton start)
  where
    go visited queue = case Seq.viewl queue of
      Seq.EmptyL -> visited
      curr Seq.:< rest ->
        if S.member curr visited
          then go visited rest
          else
            let visited' = S.insert curr visited
                neighbors = getNext curr
                newNodes = [n | n <- neighbors, not (S.member n visited')]
                queue' = rest Seq.>< Seq.fromList newNodes
             in go visited' queue'

-- | BFS複数始点版（immutable, Data.Sequence使用）
--
-- >>> import qualified Data.Set as S
-- >>> S.toList $ bfs (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> []) S.empty [1,3]
-- [1,2,3,4]
bfs :: forall a. (Ix a, Ord a) => (a -> [a]) -> S.Set a -> [a] -> S.Set a
bfs getNext initVisited initQueue = go initVisited (Seq.fromList initQueue)
  where
    go visited queue = case Seq.viewl queue of
      Seq.EmptyL -> visited
      curr Seq.:< rest ->
        if S.member curr visited
          then go visited rest
          else
            let visited' = S.insert curr visited
                neighbors = getNext curr
                newNodes = [n | n <- neighbors, not (S.member n visited')]
                queue' = rest Seq.>< Seq.fromList newNodes
             in go visited' queue'

-- | BFS単始点版（mutable, Data.Sequence使用）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> bfsSingleSourceSTUArray (1,4) (\x -> case x of 1 -> [2,3]; 2 -> [1]; 3 -> [1]; _ -> []) 1 ! 3
-- True
-- >>> bfsSingleSourceSTUArray (1,4) (\x -> case x of 1 -> [2,3]; 2 -> [1]; 3 -> [1]; _ -> []) 1 ! 4
-- False
bfsSingleSourceSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> UArray a Bool
bfsSingleSourceSTUArray bnds getNext start = runSTUArray $ do
  visited <- newArray bnds False
  queueRef <- newSTRef (Seq.singleton start)

  let loop = do
        queue <- readSTRef queueRef
        case Seq.viewl queue of
          Seq.EmptyL -> return visited
          curr Seq.:< rest -> do
            isVisited <- readArray visited curr
            if isVisited
              then do
                writeSTRef queueRef rest
                loop
              else do
                writeArray visited curr True
                let neighbors = filter (inRange bnds) (getNext curr)
                unvisitedNeighbors <- filterM (fmap not . readArray visited) neighbors
                writeSTRef queueRef $ rest Seq.>< Seq.fromList unvisitedNeighbors
                loop
  loop

-- | BFS複数始点版（mutable, Data.Sequence使用）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> bfsRunSTUArray (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> []) [1,3] ! 4
-- True
-- >>> bfsRunSTUArray (1,4) (\x -> []) [1] ! 2
-- False
bfsRunSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> UArray a Bool
bfsRunSTUArray bnds getNext initNodes = runSTUArray $ do
  visited <- newArray bnds False
  queueRef <- newSTRef (Seq.fromList initNodes)

  let loop = do
        queue <- readSTRef queueRef
        case Seq.viewl queue of
          Seq.EmptyL -> return visited
          curr Seq.:< rest -> do
            isVisited <- readArray visited curr
            if isVisited
              then do
                writeSTRef queueRef rest
                loop
              else do
                writeArray visited curr True
                let neighbors = filter (inRange bnds) (getNext curr)
                unvisitedNeighbors <- filterM (fmap not . readArray visited) neighbors
                writeSTRef queueRef $ rest Seq.>< Seq.fromList unvisitedNeighbors
                loop
  loop

-- | BFS最短経路単始点版（mutable, Data.Sequence使用）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> bfsShortestPathSTUArray (1,4) (\x -> case x of 1 -> [2]; 2 -> [3]; 3 -> [4]; _ -> []) 1 ! 4
-- 3
-- >>> bfsShortestPathSTUArray (1,4) (\x -> []) 1 ! 4
-- -1
bfsShortestPathSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> UArray a Int
bfsShortestPathSTUArray bnds getNext start = runSTUArray $ do
  distance <- newArray bnds (-1)
  queueRef <- newSTRef (Seq.singleton start)
  writeArray distance start 0

  let loop = do
        queue <- readSTRef queueRef
        case Seq.viewl queue of
          Seq.EmptyL -> return distance
          curr Seq.:< rest -> do
            currDist <- readArray distance curr
            let neighbors = filter (inRange bnds) (getNext curr)
            newNeighbors <-
              filterM
                ( \n -> do
                    d <- readArray distance n
                    if d == -1
                      then do
                        writeArray distance n (currDist + 1)
                        return True
                      else return False
                )
                neighbors
            writeSTRef queueRef $ rest Seq.>< Seq.fromList newNeighbors
            loop
  loop

-- | BFS最短経路複数始点版（mutable, Data.Sequence使用）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> bfsMultiSourceShortestPath (1,4) (\x -> case x of 1 -> [2]; 3 -> [4]; _ -> []) [1,3] ! 4
-- 1
bfsMultiSourceShortestPath :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> UArray a Int
bfsMultiSourceShortestPath bnds getNext starts = runSTUArray $ do
  distance <- newArray bnds (-1)
  queueRef <- newSTRef (Seq.fromList starts)

  mapM_ (\start -> writeArray distance start 0) starts

  let loop = do
        queue <- readSTRef queueRef
        case Seq.viewl queue of
          Seq.EmptyL -> return distance
          curr Seq.:< rest -> do
            currDist <- readArray distance curr
            let neighbors = filter (inRange bnds) (getNext curr)
            newNeighbors <-
              filterM
                ( \n -> do
                    d <- readArray distance n
                    if d == -1
                      then do
                        writeArray distance n (currDist + 1)
                        return True
                      else return False
                )
                neighbors
            writeSTRef queueRef $ rest Seq.>< Seq.fromList newNeighbors
            loop
  loop

-- | 0-1 BFS
--
-- 辺の重みが 0 か 1 のみのグラフに特化。
-- コスト0は先頭、コスト1は末尾に追加して距離順を維持する。
-- 計算量: O(V + E)
--
-- 使用例:
--   let dist = bfs01ST nextStates (0, maxState) [start]
--   where nextStates v = [(u, 0_or_1), ...]
bfs01ST :: forall a. (Ix a) => (a -> [(a, Int)]) -> (a, a) -> [a] -> UArray a Int
bfs01ST nextStates (lower, upper) v0s = runSTUArray $ do
  dist <- newArray (lower, upper) maxBound
  dequeRef <- newSTRef Seq.empty

  forM_ v0s $ \v -> do
    writeArray dist v 0
    modifySTRef' dequeRef (Seq.|> v)

  let loop = do
        deque <- readSTRef dequeRef
        case Seq.viewl deque of
          Seq.EmptyL -> return dist
          curr Seq.:< rest -> do
            writeSTRef dequeRef rest
            currDist <- readArray dist curr

            forM_ (nextStates curr) $ \(next, cost) -> do
              nextDist <- readArray dist next
              let newDist = currDist + cost
              when (newDist < nextDist) do
                writeArray dist next newDist
                if cost == 0
                  then modifySTRef' dequeRef (next Seq.<|) -- コスト0: 先頭に追加
                  else modifySTRef' dequeRef (Seq.|> next) -- コスト1: 末尾に追加
            loop

  loop

-- =============================================================================
-- DFS系関数（汎用化版）
-- =============================================================================

-- | DFS単始点版（immutable）
--
-- >>> import qualified Data.Set as S
-- >>> S.toList $ dfsSingleSource (\x -> case x of 1 -> [2,3]; 2 -> []; 3 -> []; _ -> []) S.empty 1
-- [1,2,3]
dfsSingleSource :: forall a. (Ix a, Ord a) => (a -> [a]) -> S.Set a -> a -> S.Set a
dfsSingleSource getNext visited start
  | S.member start visited = visited
  | otherwise =
      let visited' = S.insert start visited
          neighbors = getNext start
       in foldl' (dfsSingleSource getNext) visited' neighbors

-- | DFS複数始点版（immutable）
--
-- >>> import qualified Data.Set as S
-- >>> S.toList $ dfs (\x -> case x of 1 -> [2]; 3 -> [4]; _ -> []) S.empty [1,3]
-- [1,2,3,4]
dfs :: forall a. (Ix a, Ord a) => (a -> [a]) -> S.Set a -> [a] -> S.Set a
dfs getNext visited [] = visited
dfs getNext visited (curr : rest)
  | S.member curr visited = dfs getNext visited rest
  | otherwise =
      let visited' = S.insert curr visited
          neighbors = getNext curr
       in dfs getNext visited' (neighbors ++ rest)

-- | DFS単始点版（mutable）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> dfsSingleSourceSTUArray (1,4) (\x -> case x of 1 -> [2,3]; 2 -> []; 3 -> []; _ -> []) 1 ! 3
-- True
-- >>> dfsSingleSourceSTUArray (1,4) (\x -> case x of 1 -> [2]; 2 -> []; _ -> []) 1 ! 4
-- False
dfsSingleSourceSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> UArray a Bool
dfsSingleSourceSTUArray bnds getNext start = runSTUArray $ do
  visited <- newArray bnds False
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
                let neighbors = filter (inRange bnds) (getNext curr)
                modifySTRef' stackRef (neighbors ++)
                loop
  loop

-- | DFS複数始点版（mutable）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> dfsRunSTUArray (1,4) (\x -> case x of 1 -> [2]; 3 -> [4]; _ -> []) [1,3] ! 4
-- True
-- >>> dfsRunSTUArray (1,4) (\x -> []) [1] ! 2
-- False
dfsRunSTUArray :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> UArray a Bool
dfsRunSTUArray bnds getNext initNodes = runSTUArray $ do
  visited <- newArray bnds False
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
                let neighbors = filter (inRange bnds) (getNext curr)
                modifySTRef' stackRef (neighbors ++)
                loop
  loop

-- 各ノードへの経路を返す（未訪問は空リスト）
data PathNode a = PathNode a [a]

-- | DFS複数始点版・経路付き（mutable）
--
-- >>> dfsRunSTUArrayWithPath (1,3) (\x -> case x of 1 -> [2,3]; _ -> []) [1] ! 2
-- [1,2]
-- >>> dfsRunSTUArrayWithPath (1,3) (\x -> []) [1] ! 3
-- []
dfsRunSTUArrayWithPath :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [a] -> Array a [a]
dfsRunSTUArrayWithPath bnds getNext initNodes = runSTArray do
  visited <- newArray bnds False :: ST s (STUArray s a Bool)
  paths <- newArray bnds [] :: ST s (STArray s a [a])
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
                let neighbors = filter (inRange bnds) (getNext curr)
                let neighborPaths = map (\next -> PathNode next (currentPath ++ [next])) neighbors
                modifySTRef' stackRef (neighborPaths ++)
                loop
  loop

-- =============================================================================
-- 連結成分関数（Data.Sequence版）
-- =============================================================================

-- | 連結成分の個数（DFS版）
--
-- >>> countComponentsDFS (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> [])
-- 2
-- >>> countComponentsDFS (1,3) (\x -> [])
-- 3
countComponentsDFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> Int
countComponentsDFS bnds getNext = runST $ do
  visited <- newArray bnds False :: ST s (STUArray s a Bool)
  count <- newSTRef 0

  let dfs v = do
        seen <- readArray visited v
        unless seen $ do
          writeArray visited v True
          mapM_ dfs (filter (inRange bnds) (getNext v))

  forM_ (range bnds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      modifySTRef' count (+ 1)
      dfs v

  readSTRef count

-- | 連結成分の個数（BFS版, Data.Sequence使用）
--
-- >>> countComponentsBFS (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> [])
-- 2
countComponentsBFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> Int
countComponentsBFS bnds getNext = runST $ do
  visited <- newArray bnds False :: ST s (STUArray s a Bool)
  count <- newSTRef 0

  let bfs startV = do
        queue <- newSTRef (Seq.singleton startV)
        writeArray visited startV True

        let loop = do
              q <- readSTRef queue
              case Seq.viewl q of
                Seq.EmptyL -> return ()
                (v Seq.:< rest) -> do
                  writeSTRef queue rest
                  neighbors <-
                    filterM
                      ( \u -> do
                          seen <- readArray visited u
                          return (not seen)
                      )
                      (filter (inRange bnds) (getNext v))
                  mapM_ (\u -> writeArray visited u True) neighbors
                  modifySTRef' queue (Seq.>< Seq.fromList neighbors)
                  loop
        loop

  forM_ (range bnds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      modifySTRef' count (+ 1)
      bfs v

  readSTRef count

-- | 各連結成分の頂点リストを返す（DFS版）
--
-- >>> map length $ getComponentsDFS (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> [])
-- [2,2]
getComponentsDFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [[a]]
getComponentsDFS bnds getNext = runST do
  visited <- newArray bnds False :: ST s (STUArray s a Bool)
  components <- newSTRef []

  let dfs v acc = do
        seen <- readArray visited v
        if not seen
          then do
            writeArray visited v True
            foldM (flip dfs) (v : acc) (filter (inRange bnds) (getNext v))
          else return acc

  forM_ (range bnds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      component <- dfs v []
      unless (null component) $
        modifySTRef' components (component :)

  readSTRef components

-- | 各連結成分の頂点リストを返す（BFS版）
--
-- >>> map length $ getComponentsBFS (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> [])
-- [2,2]
getComponentsBFS :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [[a]]
getComponentsBFS bnds getNext = runST $ do
  visited <- newArray bnds False :: ST s (STUArray s a Bool)
  components <- newSTRef []

  let bfs startV = do
        queue <- newSTRef (Seq.singleton startV)
        component <- newSTRef []
        writeArray visited startV True
        modifySTRef' component (startV :)

        let loop = do
              q <- readSTRef queue
              case Seq.viewl q of
                Seq.EmptyL -> return ()
                v Seq.:< rest -> do
                  writeSTRef queue rest
                  neighbors <-
                    filterM
                      ( \u -> do
                          seen <- readArray visited u
                          return (not seen)
                      )
                      (filter (inRange bnds) (getNext v))
                  mapM_
                    ( \u -> do
                        writeArray visited u True
                        modifySTRef' component (u :)
                    )
                    neighbors
                  modifySTRef' queue (Seq.>< Seq.fromList neighbors)
                  loop

        loop
        readSTRef component

  forM_ (range bnds) $ \v -> do
    seen <- readArray visited v
    unless seen $ do
      component <- bfs v
      modifySTRef' components (component :)

  readSTRef components

-- | 各連結成分のサイズリストを返す
--
-- >>> getComponentSizes (1,5) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4,5]; 4 -> [3]; 5 -> [3]; _ -> [])
-- [3,2]
getComponentSizes :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> [Int]
getComponentSizes bnds getNext = map length (getComponentsDFS bnds getNext)

-- | 最大連結成分のサイズを返す
--
-- >>> getLargestComponent (1,5) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4,5]; 4 -> [3]; 5 -> [3]; _ -> [])
-- 3
getLargestComponent :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> Int
getLargestComponent bnds getNext =
  let sizes = getComponentSizes bnds getNext
   in if null sizes then 0 else maximum sizes

-- =============================================================================
-- 特殊関数（汎用化版）
-- =============================================================================

-- | 指定ノードが属する連結成分のサイズを返す
--
-- >>> getComponentSize (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> []) 1
-- 2
getComponentSize :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> Int
getComponentSize bnds getNext v = runST $ do
  visited <- newArray bnds False :: ST s (STUArray s a Bool)
  size <- newSTRef 0

  let dfs u = do
        seen <- readArray visited u
        unless seen $ do
          writeArray visited u True
          modifySTRef' size (+ 1)
          mapM_ dfs (filter (inRange bnds) (getNext u))

  if inRange bnds v
    then do
      dfs v
      readSTRef size
    else return 0

-- | 2頂点が同じ連結成分に属するか判定する
--
-- >>> isConnected (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> []) 1 2
-- True
-- >>> isConnected (1,4) (\x -> case x of 1 -> [2]; 2 -> [1]; 3 -> [4]; 4 -> [3]; _ -> []) 1 3
-- False
isConnected :: forall a. (Ix a) => (a, a) -> (a -> [a]) -> a -> a -> Bool
isConnected bnds getNext u v
  | not (inRange bnds u) || not (inRange bnds v) = False
  | u == v = True
  | otherwise = runST $ do
      visited <- newArray bnds False :: ST s (STUArray s a Bool)
      found <- newSTRef False

      let dfs curr = do
            when (curr == v) $ writeSTRef found True
            seen <- readArray visited curr
            foundFlag <- readSTRef found
            when (not seen && not foundFlag) $ do
              writeArray visited curr True
              mapM_ dfs (filter (inRange bnds) (getNext curr))

      dfs u
      readSTRef found

-- =============================================================================
-- Array版（便利関数）
-- =============================================================================

-- | Array版：連結成分数（DFS）
--
-- >>> countComponentsArrayDFS (buildG2 (1,4) [[1,2],[3,4]])
-- 2
countComponentsArrayDFS :: forall a. (Ix a) => Array a [a] -> Int
countComponentsArrayDFS graph =
  let bnds' = bounds graph
      getNext v = graph ! v
   in countComponentsDFS bnds' getNext

-- | Array版：連結成分数（BFS）
--
-- >>> countComponentsArrayBFS (buildG2 (1,4) [[1,2],[3,4]])
-- 2
countComponentsArrayBFS :: forall a. (Ix a) => Array a [a] -> Int
countComponentsArrayBFS graph =
  let bnds' = bounds graph
      getNext v = graph ! v
   in countComponentsBFS bnds' getNext

-- | Array版：連結成分リスト
--
-- >>> map length $ getComponentsArray (buildG2 (1,4) [[1,2],[3,4]])
-- [2,2]
getComponentsArray :: forall a. (Ix a) => Array a [a] -> [[a]]
getComponentsArray graph =
  let bnds' = bounds graph
      getNext v = graph ! v
   in getComponentsDFS bnds' getNext

-- =============================================================================
-- グリッド系（汎用化版）
-- =============================================================================

-- | グリッドの構築（行リストから UArray を作る）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> buildGrid ((0,0),(1,1)) ["ab","cd"] ! (0,1)
-- 'b'
buildGrid :: (IArray UArray e, Ix i, Foldable t) => (i, i) -> t [e] -> UArray i e
buildGrid bnds lst = listArray @UArray bnds $ concat lst

-- | グリッド上の隣接座標を取得する（4方向版）
--
-- >>> import Data.Array.Unboxed (listArray)
-- >>> let g = listArray ((0,0),(2,2)) (replicate 9 '.') :: UArray (Int,Int) Char
-- >>> getNextGrid4 g '#' (1,1)
-- [(0,1),(2,1),(1,0),(1,2)]
getNextGrid4 :: UArray (Int, Int) Char -> Char -> (Int, Int) -> [(Int, Int)]
getNextGrid4 grid wallChar (r, c) =
  let bnds' = bounds grid
      directions = [(-1, 0), (1, 0), (0, -1), (0, 1)] -- 上下左右
      neighbors = [(r + dr, c + dc) | (dr, dc) <- directions]
      validNeighbors =
        [ (nr, nc)
          | (nr, nc) <- neighbors,
            inRange bnds' (nr, nc),
            grid ! (nr, nc) /= wallChar -- 塀以外
        ]
   in validNeighbors

-- | グリッド上で2マスが4方向隣接しているか
--
-- >>> isAdj4 (1, 1) (1, 2)
-- True
-- >>> isAdj4 (1, 1) (2, 2)
-- False
-- >>> isAdj4 (1, 1) (1, 1)
-- False
isAdj4 :: (Int, Int) -> (Int, Int) -> Bool
isAdj4 (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) == 1

-- | グリッド上で2マスが8方向隣接しているか（同一マスは False）
--
-- >>> isAdj8 (1, 1) (2, 2)
-- True
-- >>> isAdj8 (1, 1) (1, 1)
-- False
isAdj8 :: (Int, Int) -> (Int, Int) -> Bool
isAdj8 (r1, c1) (r2, c2) =
  let dr = abs (r1 - r2)
      dc = abs (c1 - c2)
   in max dr dc == 1

-- | グリッド上の隣接座標を取得する（8方向版）
--
-- >>> import Data.Array.Unboxed (listArray)
-- >>> let g = listArray ((0,0),(2,2)) (replicate 9 '.') :: UArray (Int,Int) Char
-- >>> length (getNextGrid8 g '#' (1,1))
-- 8
getNextGrid8 :: UArray (Int, Int) Char -> Char -> (Int, Int) -> [(Int, Int)]
getNextGrid8 grid wallChar (r, c) =
  let bnds' = bounds grid
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
        [ (nr, nc) | (nr, nc) <- neighbors, inRange bnds' (nr, nc), grid ! (nr, nc) /= wallChar -- 境界チェック追加
        -- 塀以外
        ]
   in validNeighbors

-- | 距離の2乗がちょうど m になる移動先を列挙する（1≤座標≤n の範囲内）
--
-- >>> getNextMoves 5 2 (3,3)
-- [(2,2),(2,4),(4,2),(4,4)]
getNextMoves :: Int -> Int -> (Int, Int) -> [(Int, Int)]
getNextMoves n m (r, c) =
  [ (nr, nc)
    | dr <- [-n .. n],
      dc <- [-n .. n],
      dr * dr + dc * dc == m,
      let nr = r + dr,
      let nc = c + dc,
      inRange (1, n) nr,
      inRange (1, n) nc
  ]

-- | グリッド版: 特定文字の連結成分数（DFS）
--
-- >>> import Data.Array.Unboxed (listArray)
-- >>> let g = listArray ((0,0),(1,3)) ".#.#.#.#" :: UArray (Int,Int) Char
-- >>> countGridComponentsDFS g '.' (\_ -> [])
-- 4
countGridComponentsDFS :: forall a. (Ix a) => UArray a Char -> Char -> (a -> [a]) -> Int
countGridComponentsDFS grid targetChar getNext = runST $ do
  let bnds' = bounds grid
  visited <- newArray bnds' False :: ST s (STUArray s a Bool)
  count <- newSTRef 0

  let targetPositions = [pos | pos <- range bnds', grid ! pos == targetChar]

  let dfs v = do
        seen <- readArray visited v
        unless seen $ do
          writeArray visited v True
          mapM_ dfs (getNext v)

  forM_ targetPositions $ \pos -> do
    seen <- readArray visited pos
    unless seen $ do
      modifySTRef' count (+ 1)
      dfs pos

  readSTRef count

-- | グリッド版: 特定文字の連結成分数（BFS）
--
-- >>> import Data.Array.Unboxed (listArray)
-- >>> let g = listArray ((0,0),(1,3)) ".#.#.#.#" :: UArray (Int,Int) Char
-- >>> countGridComponentsBFS g '.' (\_ -> [])
-- 4
countGridComponentsBFS :: forall a. (Ix a) => UArray a Char -> Char -> (a -> [a]) -> Int
countGridComponentsBFS grid targetChar getNext = runST $ do
  let bnds' = bounds grid
  visited <- newArray bnds' False :: ST s (STUArray s a Bool)
  count <- newSTRef 0

  let targetPositions = [pos | pos <- range bnds', grid ! pos == targetChar]

  let bfs startV = do
        queue <- newSTRef (Seq.singleton startV)
        writeArray visited startV True

        let loop = do
              q <- readSTRef queue
              case Seq.viewl q of
                Seq.EmptyL -> return ()
                v Seq.:< rest -> do
                  writeSTRef queue rest
                  neighbors <-
                    filterM
                      ( \u -> do
                          seen <- readArray visited u
                          return (not seen)
                      )
                      (getNext v)
                  mapM_ (\u -> writeArray visited u True) neighbors
                  modifySTRef' queue (Seq.>< Seq.fromList neighbors)
                  loop
        loop

  forM_ targetPositions $ \pos -> do
    seen <- readArray visited pos
    unless seen $ do
      modifySTRef' count (+ 1)
      bfs pos

  readSTRef count

printIntGrid :: (Show e, IArray a e, Ix v) => a (v, Int) e -> IO ()
printIntGrid grid = traverse_ (putStrLn . unwords . map show) $ chunksOf w (elems grid)
  where
    ((_, w1), (_, w2)) = bounds grid
    w = w2 + 1 - w1

data LRUD = L | R | U | D deriving (Show, Eq, Ord, Ix, Enum)

lrud@[left, right, up, down] = [(0, -1), (0, 1), (-1, 0), (1, 0)]

-- | グリッドの連結成分数（'.'の島の数を数える用途など）
--
-- >>> import Data.Array.Unboxed (listArray)
-- >>> let g = listArray ((0,0),(1,3)) ".#.#.#.#" :: UArray (Int,Int) Char
-- >>> countGridIslands g (\_ -> [])
-- 8
countGridIslands :: forall a. (Ix a) => UArray a Char -> (a -> [a]) -> Int
countGridIslands grid getNext =
  let bnds' = bounds grid
   in countComponentsDFS bnds' getNext

-- =============================================================================
-- 強連結成分分解 (SCC) — Kosaraju のアルゴリズム
-- =============================================================================

-- | 隣接関数版: 強連結成分を求める（Kosaraju のアルゴリズム）
--
-- 1. 元のグラフで DFS し、帰りがけ順を記録
-- 2. 逆辺グラフ上で、帰りがけ順の逆順に DFS → 各連結成分が SCC
--
-- >>> sccFromAdj (1,3) (\v -> case v of 1 -> [2]; 2 -> [3]; 3 -> [1]; _ -> [])
-- [[2,3,1]]
-- >>> length $ sccFromAdj (1,3) (\v -> case v of 1 -> [2]; _ -> [])
-- 3
sccFromAdj :: (Int, Int) -> (Int -> [Int]) -> [[Int]]
sccFromAdj bnds getNext = runST do
  let (lo, hi) = bnds

  -- Step 1: 元グラフで DFS、帰りがけ順を求める
  visited1 <- newArray bnds False :: ST s (STUArray s Int Bool)
  orderRef <- newSTRef ([] :: [Int])

  let dfs1 v = do
        seen <- readArray visited1 v
        unless seen do
          writeArray visited1 v True
          forM_ (getNext v) $ \u ->
            when (u >= lo && u <= hi) (dfs1 u)
          modifySTRef' orderRef (v :)

  forM_ (range bnds) dfs1

  -- 逆辺グラフを構築
  let revAdj :: Array Int [Int]
      revAdj =
        accumArray
          (flip (:))
          []
          bnds
          [(u, v) | v <- range bnds, u <- getNext v, u >= lo && u <= hi]

  -- Step 2: 帰りがけ順の逆順（= orderRef そのまま）で逆辺グラフを DFS
  visited2 <- newArray bnds False :: ST s (STUArray s Int Bool)
  compsRef <- newSTRef ([] :: [[Int]])
  order <- readSTRef orderRef

  let dfs2 v compRef = do
        seen <- readArray visited2 v
        unless seen do
          writeArray visited2 v True
          modifySTRef' compRef (v :)
          forM_ (revAdj ! v) (`dfs2` compRef)

  forM_ order $ \v -> do
    seen <- readArray visited2 v
    unless seen do
      compRef <- newSTRef []
      dfs2 v compRef
      comp <- readSTRef compRef
      modifySTRef' compsRef (comp :)

  readSTRef compsRef

-- | 隣接関数版: 各 SCC のサイズ
--
-- >>> sccSizesFromAdj (1,3) (\v -> case v of 1 -> [2]; 2 -> [3]; 3 -> [1]; _ -> [])
-- [3]
sccSizesFromAdj :: (Int, Int) -> (Int -> [Int]) -> [Int]
sccSizesFromAdj bnds getNext = map length (sccFromAdj bnds getNext)

-- | 隣接関数版: 互いに到達可能なペア数（サイズ k の SCC から k*(k-1)/2 個）
--
-- >>> sccPairCountFromAdj (1,3) (\v -> case v of 1 -> [2]; 2 -> [3]; 3 -> [1]; _ -> [])
-- 3
sccPairCountFromAdj :: (Int, Int) -> (Int -> [Int]) -> Integer
sccPairCountFromAdj bnds getNext =
  sum [k * (k - 1) `div` 2 | s <- sccFromAdj bnds getNext, let k = toInteger (length s)]

-- | 辺リスト [[from, to]] から強連結成分を求める（頂点は 1..n）
--
-- >>> sccG 3 [[1,2],[2,3],[3,1]]
-- [[2,3,1]]
sccG :: Int -> [[Int]] -> [[Int]]
sccG n edges = sccFromAdj (1, n) (adj !)
  where
    adj :: Array Int [Int]
    adj = accumArray (flip (:)) [] (1, n) [(a, b) | [a, b] <- edges]

-- | 辺リストから各 SCC のサイズ
--
-- >>> sccSizes 3 [[1,2],[2,3],[3,1]]
-- [3]
sccSizes :: Int -> [[Int]] -> [Int]
sccSizes n edges = map length (sccG n edges)

-- | 辺リストから互いに到達可能なペア数
--
-- >>> sccPairCount 3 [[1,2],[2,3],[3,1]]
-- 3
sccPairCount :: Int -> [[Int]] -> Integer
sccPairCount n edges = sum [k * (k - 1) `div` 2 | s <- sccG n edges, let k = toInteger (length s)]

-- | Array版: 有向グラフの強連結成分を求める
--
-- >>> length $ sccArray (buildG (1,3) [[1,2],[2,3],[3,1]])
-- 1
sccArray :: Array Int [Int] -> [[Int]]
sccArray graph = sccFromAdj (bounds graph) (graph !)

-- =============================================================================
-- 木DP系関数
-- =============================================================================

-- | 根付き木の各頂点の部分木サイズを求める（隣接関数版）
--
-- 使用例:
--   let graph = buildG2 (1, n) edges
--       sz = subtreeSizes (1, n) (graph !) 1
--   sz ! v == 頂点vを根とする部分木の頂点数
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> let adj v = case v of 1 -> [2,3]; 2 -> [1]; 3 -> [1]; _ -> []
-- >>> subtreeSizes (1,3) adj 1 ! 1
-- 3
-- >>> subtreeSizes (1,3) adj 1 ! 2
-- 1
subtreeSizes :: (Int, Int) -> (Int -> [Int]) -> Int -> UArray Int Int
subtreeSizes bnds getNext root = runSTUArray do
  sz <- newArray bnds 1 :: ST s (STUArray s Int Int)
  visited <- newArray bnds False :: ST s (STUArray s Int Bool)
  orderRef <- newSTRef ([] :: [(Int, Int)])
  stackRef <- newSTRef [(root, -1 :: Int)]

  -- DFSで訪問順を記録
  let loop = do
        stack <- readSTRef stackRef
        case stack of
          [] -> return ()
          ((v, p) : rest) -> do
            writeSTRef stackRef rest
            vis <- readArray visited v
            unless vis do
              writeArray visited v True
              modifySTRef' orderRef ((v, p) :)
              forM_ (getNext v) $ \c ->
                when (c /= p) $ modifySTRef' stackRef ((c, v) :)
            loop
  loop

  -- 訪問順の逆順（＝帰りがけ順）で部分木サイズを集計
  order <- readSTRef orderRef
  forM_ order $ \(v, p) ->
    when (p /= -1) do
      sv <- readArray sz v
      modifyArray' sz p (+ sv)

  return sz

-- | 根付き木の各頂点の部分木サイズを求める（Array版）
--
-- >>> import Data.Array.Unboxed ((!))
-- >>> subtreeSizesArray (buildG2 (1,3) [[1,2],[1,3]]) 1 ! 1
-- 3
subtreeSizesArray :: Array Int [Int] -> Int -> UArray Int Int
subtreeSizesArray graph = subtreeSizes (bounds graph) (graph !)

-- =============================================================================
-- 汎用木DP
-- =============================================================================

-- | 純粋再帰版 木DP（根のDP結果のみ返す）
--
--   graph    : 無向グラフ（木）
--   root     : 根の頂点
--   initState: 頂点 → 初期状態
--   merge    : 親の累積状態 → 子のDP結果 → 新しい親の状態
--
--   ※ チェーン状の木（深さ N）でスタック溢れの可能性あり
--     → +RTS -K1G で回避、または treeDPBFS を使う
--
-- >>> treeDFS (buildG2 (1,4) [[1,2],[1,3],[3,4]]) 1 (const 1) (+)
-- 4
treeDFS :: Array Int [Int] -> Int -> (Int -> a) -> (a -> a -> a) -> a
treeDFS graph root initState merge = go (-1) root
  where
    go p v = foldl' merge (initState v) [go v u | u <- graph ! v, u /= p]

-- | BFS逆順版 木DP（全頂点のDP結果を返す、スタック安全）
--
--   O(N) 時間・空間。チェーン状の木でも安全。
--   返り値は Array なので dp ! v で各頂点の結果を取得可能。
--
-- >>> treeDPBFS (buildG2 (1,4) [[1,2],[1,3],[3,4]]) 1 (const 1) (+) ! 1
-- 4
-- >>> treeDPBFS (buildG2 (1,4) [[1,2],[1,3],[3,4]]) 1 (const 1) (+) ! 3
-- 2
treeDPBFS :: Array Int [Int] -> Int -> (Int -> a) -> (a -> a -> a) -> Array Int a
treeDPBFS graph root initState merge = runSTArray do
  let bnds = bounds graph

  -- BFS で訪問順と親を記録
  parent <- newArray bnds (-1) :: ST s (STUArray s Int Int)
  visited <- newArray bnds False :: ST s (STUArray s Int Bool)
  orderRef <- newSTRef (Seq.empty :: Seq.Seq Int)
  queueRef <- newSTRef (Seq.singleton root)
  writeArray visited root True

  let bfsLoop = do
        q <- readSTRef queueRef
        case Seq.viewl q of
          Seq.EmptyL -> return ()
          v Seq.:< rest -> do
            writeSTRef queueRef rest
            modifySTRef' orderRef (Seq.|> v)
            forM_ (graph ! v) \u -> do
              seen <- readArray visited u
              unless seen do
                writeArray visited u True
                writeArray parent u v
                modifySTRef' queueRef (Seq.|> u)
            bfsLoop
  bfsLoop

  -- DP 配列を初期化
  dp <- newListArray bnds [initState v | v <- range bnds]

  -- 逆BFS順（葉 → 根）でマージ
  order <- toList <$> readSTRef orderRef
  forM_ (reverse order) \v -> do
    p <- readArray parent v
    when (p /= -1) do
      pVal <- readArray dp p
      vVal <- readArray dp v
      writeArray dp p (merge pVal vVal)
  return dp

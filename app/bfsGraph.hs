import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- 幅優先探索（グラフ用）
type Graph = Array Int [Int]

type Edge = (Int, Int)

type Bounds = (Int, Int)

-- グラフ構築
buildG :: Bounds -> [[Int]] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v), (v, u)]) edges

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
          next = g A.! q
          queue' = queue Seq.>< Seq.fromList (L.map (,succ l) next)
       in bfs n g queue' visited'
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

-- STArray版 速い
-- BFSを実行し、始点からの距離をUArrayで返す
bfs :: Graph -> Bounds -> Int -> UArray Int Int
bfs graph (l, h) start = runSTUArray $ do
  -- 距離配列を-1で初期化
  dist <- newArray (l, h) (-1) :: ST s (STUArray s Int Int)
  -- 探索キュー
  queue <- newSTRef (Seq.singleton start)
  -- 始点の距離を0に設定
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
        forM_ (graph ! v) $ \u -> do
          uDist <- readArray dist u
          when (uDist == -1) $ do
            writeArray dist u (currentDist + 1)
            modifySTRef' queue (Seq.>< Seq.singleton u)
        loop

  return dist

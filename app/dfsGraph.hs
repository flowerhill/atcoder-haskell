import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

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
dfs2 :: Eq p => (p -> [p]) -> p -> p -> [[p]]
dfs2 next start goal = go [start]
  where
    go [] = []
    go path@(head : tail)
      | head == goal = return path
      | otherwise = do
          guard $ head `notElem` tail
          u <- next head
          go (u : path)

getInts :: IO [Int]
getInts = unfoldr (BC.readInt . BC.dropWhile C.isSpace) <$> BC.getLine

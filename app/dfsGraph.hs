import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

-- 深さ優先探索（グラフ用）
type Graph = Array Int [Int]

type Edge = (Int, Int)

type Bounds = (Int, Int)

-- グラフ構築(無向グラフ)
buildG :: Bounds -> [[Int]] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds edges'
  where
    edges' = concatMap (\[u, v] -> [(u, v), (v, u)]) edges

dfs :: Graph -> IS.IntSet -> Int -> IS.IntSet
dfs g seen v
  | IS.member v seen = seen
  | otherwise = foldl' (dfs g) seen' next_vs
  where
    next_vs = g ! v
    seen' = IS.insert v seen
type Vertex = Int

type Weight = Int

type Edge = (Vertex, Weight)

type Graph = Array Vertex [Edge]

type Node = H.Entry Int Vertex

type Queue = H.Heap Node

type Dist = IM.IntMap Int

type Bounds = (Int, Int)

buildG :: Bounds -> [[Int]] -> Graph
buildG (start, end) edges = accumArray (flip (:)) [] (start, end) edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w))]) edges
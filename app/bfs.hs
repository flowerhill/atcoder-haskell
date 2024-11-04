import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- 幅優先探索
type Graph = [(Int, [Int])]

bfs :: Graph -> Int -> [Int]
bfs graph start = inner graph (Seq.singleton start) S.empty []
  where
    inner :: Graph -> Seq.Seq Int -> S.Set Int -> [Int] -> [Int]
    inner graph queue visited result
      | Seq.null queue = reverse result
      | otherwise =
          let (current Seq.:< rest) = Seq.viewl queue
              neighbors = fromMaybe [] (lookup current graph)
              unvisitedNeighbors = filter (\n -> not $ S.member n visited) neighbors
              newQueue = rest Seq.>< Seq.fromList unvisitedNeighbors
              newVisited = S.insert current visited
              newResult = current : result
           in inner graph newQueue newVisited newResult

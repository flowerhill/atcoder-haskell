{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import qualified Data.Heap as H
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe

-- dijkstra用のgraph
type Vertex = Int

type Weight = Int

type Edge = (Vertex, Weight)

type Graph = Array Vertex [Edge]

type Node = H.Entry Int Vertex

type Queue = H.Heap Node

type Dist = IM.IntMap Int

type Bounds = (Int, Int)

buildGraph :: Bounds -> [[Int]] -> Graph
buildGraph (start, end) edges = accumArray (flip (:)) [] (start, end) edges'
  where
    edges' = concatMap (\[u, v, w] -> [(u, (v, w))]) edges

-- immutable版 重いのであまり使えない

dijkstra :: Graph -> Vertex -> Int -> Dist
dijkstra g v0 n = f (Just (start, H.empty)) IS.empty dist0
  where
    start :: Node
    start = H.Entry 0 v0

    dist0 :: Dist
    dist0 = IM.fromList $ (v0, 0) : [(k, maxBound :: Int) | k <- [2 .. n]]

    f :: Maybe (Node, Queue) -> IS.IntSet -> Dist -> Dist
    f Nothing _ dist = dist
    f (Just (H.Entry currentCost i, q)) done dist
      | IS.member i done = f (H.uncons q) done dist
      | otherwise = f (H.uncons q') done' dist'
      where
        done' = IS.insert i done
        edges = g ! i
        (q', dist') = foldl' (relax currentCost) (q, dist) edges

    relax :: Int -> (Queue, Dist) -> Edge -> (Queue, Dist)
    relax currentCost (q, dist) (v, w) = (q', dist')
      where
        cost = currentCost + w
        dist' = IM.adjust (`min` cost) v dist
        q'
          | cost < dist IM.! v = H.insert (H.Entry cost v) q
          | otherwise = q

--  mutable版 基本はこっちを使う
dijkstraST :: Ix v => (v -> [(v, Int)]) -> (v, v) -> [v] -> UArray v Int
dijkstraST nextStates (lower, upper) v0s = runSTUArray $ do
  dist <- newArray (lower, upper) maxBound

  forM_ v0s $ \v -> do
    writeArray dist v 0

  let queue = H.fromList $ map (H.Entry 0) v0s

  aux (H.uncons queue) dist
  return dist
  where
    aux Nothing _ = return ()
    aux (Just (H.Entry dv v, queue)) dist = do
      garbage <- (dv >) <$> readArray dist v

      if garbage
        then aux (H.uncons queue) dist -- skip
        else do
          queue' <-
            foldM
              ( \q (u, w) -> do
                  du <- readArray dist u

                  let dv' = dv + w

                  if dv' < du
                    then do
                      writeArray dist u dv'
                      return $ H.insert (H.Entry dv' u) q
                    else return q
              )
              queue
              (nextStates v)

          aux (H.uncons queue') dist

{-- 純粋関数型実装（Map ベース）--}
-- Union-Find data type
type UnionFind = IOUArray Int Int

-- Initialize Union-Find
initUnionFind :: Int -> Int -> [Int] -> IO UnionFind
initUnionFind l h lst = newListArray (l, h) lst

-- Mutable Union-Find with IOUArray
findRoot :: IOUArray Int Int -> Int -> IO Int
findRoot uf x = do
  parent <- readArray uf x
  if parent == x
    then return x
    else do
      root <- findRoot uf parent
      writeArray uf x root -- Path compression
      return root

-- Unite two components
unite :: IOUArray Int Int -> Int -> Int -> IO ()
unite uf x y = do
  rootX <- findRoot uf x
  rootY <- findRoot uf y
  when (rootX /= rootY) $ do
    writeArray uf rootX rootY

-- Check if two nodes are in the same component
isConnected :: IOUArray Int Int -> Int -> Int -> IO Bool
isConnected uf x y = do
  rootX <- findRoot uf x
  rootY <- findRoot uf y
  return (rootX == rootY)
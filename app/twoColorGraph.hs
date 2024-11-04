{- 二部グラフ -}
-- 二部グラフ彩色DFS
dfsM :: (Foldable t2, MArray a Bool m, Ix t1) => (t1 -> t2 t1) -> a t1 Bool -> [(t1, Bool)] -> (t1, Bool) -> m [(t1, Bool)]
dfsM nextStates visited path (v, color) = do
  writeArray visited v True
  foldM f ((v, color) : path) (nextStates v)
  where
    f context u = do
      seen <- readArray visited u
      if seen
        then return context
        else dfsM nextStates visited context (u, not color)

-- 連結している成分を返し、カラーリングする
componentsM :: (MArray IOUArray Bool m, Foldable t, Ix v) => (v -> [v]) -> (v, v) -> t v -> m [[(v, Bool)]]
componentsM nextStates v vs = do
  visited <- newArray @IOUArray v False
  foldM (f visited) [] vs
  where
    f visited cs u = do
      seen <- readArray visited u
      if seen
        then return cs
        else do
          path <- dfsM nextStates visited [] (u, True)
          return (path : cs)

-- 二部グラフ判定
isBipartite :: (Foldable t, Eq a, IArray UArray a, Ix i, Num i) => i -> (i -> t i) -> [(i, a)] -> Bool
isBipartite n nextStates colored = do
  let cs = array @UArray (1, n) colored
  all
    ( \(v, color) ->
        all (\u -> cs ! u /= color) (nextStates v)
    )
    colored

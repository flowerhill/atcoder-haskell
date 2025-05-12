-- mutable版bfs
bfs :: (Ix v) => (v -> [v]) -> Int -> (v, v) -> [(v, Int)] -> UArray v Int
bfs nextStates initial b v0s = runSTUArray $ do
  dist <- newArray b initial

  for_ v0s $ \(v0, d0) -> do
    writeArray dist v0 d0

  aux (Seq.fromList [v0 | (v0, _) <- v0s]) dist
  return dist
  where
    aux Empty _ = return ()
    aux (v :<| queue) dist = do
      d <- readArray dist v
      us <- filterM (fmap (== initial) . readArray dist) (nextStates v)

      queue' <- foldForM queue us $ \q u -> do
        writeArray dist u (d + 1)
        return $ q |> u

      aux queue' dist

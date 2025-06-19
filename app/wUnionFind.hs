{-- 重み付き Union-Find --}
-- ref: https://qiita.com/drken/items/cce6fc5c579051e64fab
data WeightedUnionFind a v
  = WeightedUnionFind
      (a v v) -- 親頂点 / -1 は代表元
      (IOUArray v Int) -- 集合サイズ (代表元で検索する)
      (IOUArray v Int) -- weight
      (IORef Int) -- 連結成分数
      v -- 代表元 (representative element)

newWUF :: (MArray a1 a2 IO, Ix a2) => (a2, a2) -> a2 -> IO (WeightedUnionFind a1 a2)
newWUF (l, u) rep =
  WeightedUnionFind
    <$> newArray (l, u) rep
    <*> newArray (l, u) 1
    <*> newArray (l, u) 0
    <*> newIORef (bool 0 (ix u + 1 - ix l) (u >= l))
    <*> pure rep
  where
    ix = index (l, u)

rootWUF :: (Ix i, MArray a i m, MArray IOUArray Int m) => WeightedUnionFind a i -> i -> m i
rootWUF uf@(WeightedUnionFind parent _ weight _ rep) x = do
  p <- readArray parent x
  if p == rep
    then return x
    else do
      r <- rootWUF uf p
      writeArray parent x r

      -- 累積和
      w <- readArray weight p
      updateArray (+) weight x w

      return r

getWeightWUF :: (Ix i, MArray a i m, MArray IOUArray Int m) => WeightedUnionFind a i -> i -> m Int
getWeightWUF uf@(WeightedUnionFind _ _ weight _ _) x = do
  _ <- rootWUF uf x -- 経路圧縮
  readArray weight x

uniteWUF :: (Ix i, MArray a i IO) => WeightedUnionFind a i -> i -> i -> Int -> IO ()
uniteWUF uf@(WeightedUnionFind parent size weight refN _) x y w = do
  x' <- rootWUF uf x
  y' <- rootWUF uf y

  wx <- getWeightWUF uf x
  wy <- getWeightWUF uf y

  let w' = w + wx - wy

  when (x' /= y') $ do
    sizeX <- readArray size x'
    sizeY <- readArray size y'

    -- 併合する毎に一つ連結成分数が減る
    modifyIORef' refN (+ (-1))

    if sizeX > sizeY
      then do
        writeArray size x' (sizeX + sizeY)
        writeArray parent y' x'
        writeArray weight y' w'
      else do
        writeArray size y' (sizeX + sizeY)
        writeArray parent x' y'
        writeArray weight x' (negate w')

isSameWUF :: (Ix a1, MArray a2 a1 f, MArray IOUArray Int f) => WeightedUnionFind a2 a1 -> a1 -> a1 -> f Bool
isSameWUF uf x y = (==) <$> rootWUF uf x <*> rootWUF uf y
{-- MArray --}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray ary ix f = do
  v <- readArray ary ix
  writeArray ary ix $! f v
{-# INLINE modifyArray #-}

swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray as i j = do
  a <- readArray as i
  b <- readArray as j
  writeArray as j $! a
  writeArray as i $! b
{-# INLINE swapArray #-}

updateArray :: (MArray a t m, Ix i) => a i t -> (t -> t) -> i -> m ()
updateArray arr f i = do
  val <- readArray arr i
  writeArray arr i (f val)
{-# INLINE updateArray #-}

updateArray2 :: (MArray a e m, Ix i) => (e -> e' -> e) -> a i e -> i -> e' -> m ()
updateArray2 f arr ix x = do
  v <- readArray arr ix
  writeArray arr ix $! f v x
{-# INLINE updateArray2 #-}

{-- IOArray --}

getRowAsArray :: Int -> Int -> IOUArray (Int, Int) Int -> IO (IOUArray Int Int)
getRowAsArray n w arr = do
  -- 新しい一次元配列を作成
  newArr <- newArray (0, w) minBound :: IO (IOUArray Int Int)
  -- 元の配列から値を読み取り、新しい配列に書き込む
  mapM_
    ( \col -> do
        val <- readArray arr (n, col)
        writeArray newArr col val
    )
    [0 .. w]
  return newArr

{-- grid --}

printMatrix :: UArray (Int, Int) Int -> IO ()
printMatrix arr = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds arr
  let rows = [[arr ! (row, col) | col <- [minCol .. maxCol - 1]] | row <- [minRow .. maxRow - 1]]
  putStr $ unlines [unwords (map show row) | row <- rows]

twoDimensionalSum :: UArray (Int, Int) Int -> UArray (Int, Int) Int
twoDimensionalSum arr =
  listArray bounds_ $
    concat $
      scanl1 (zipWith (+)) $
        map (scanl1 (+)) lists
  where
    bounds_ = bounds arr
    lists = LS.chunksOf ((snd . snd) bounds_) $ elems arr
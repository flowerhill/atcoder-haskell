b1Hash = 1676943010

m1Hash = 2147483647

b2Hash = 1020544910

m2Hash = 2147483647

scanHash :: Int -> Int -> String -> Int -> IA.Array Int Int
scanHash b m t n = IA.listArray (0, n) $ L.scanl' (\acc c -> plusMod m (mulMod m acc b) (toInt c)) 0 t :: IA.Array Int Int
  where
    toInt c = C.ord c - C.ord 'a' + 1

rangeHash :: Int -> Int -> IA.Array Int Int -> Int -> Int -> Int
rangeHash b m arr l r = if val >= 0 then val else val + m
  where
    hr = arr IA.! (r + 1)
    hl = arr IA.! l
    val = minusMod m hr (mulMod m (powertMod m b (r - l + 1)) hl)

plusMod :: Int -> Int -> Int -> Int
plusMod m x y = (x + y) `mod` m

minusMod :: Int -> Int -> Int -> Int
minusMod m x y
  | x < y = m + x - y
  | otherwise = x - y

mulMod :: Int -> Int -> Int -> Int
mulMod m x y = (x * y) `mod` m

powertMod :: Int -> Int -> Int -> Int
powertMod m x 0 = 1
powertMod m x y
  | even y = mulMod m t t
  | otherwise = mulMod m x $ mulMod m t t
  where
    t = powertMod m x $ y `div` 2
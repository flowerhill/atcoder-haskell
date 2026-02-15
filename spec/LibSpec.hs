{-# LANGUAGE ImportQualifiedPost #-}

module LibSpec (spec) where

import Data.Containers.ListUtils (nubOrd)
import Data.List
import Data.Set qualified as S
import Lib (solve)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "solve" $ do
    it "配列の合計を計算" $ do
      solve 3 [1, 2, 3] `shouldBe` 6

    it "空配列は0" $ do
      solve 0 [] `shouldBe` 0

--------------------------------------------------------------------------------
-- 基本的な数値ジェネレータ
--------------------------------------------------------------------------------

-- | 1以上N以下の整数
genRange :: Int -> Int -> Gen Int
genRange lo hi = choose (lo, hi)

-- | 小さい正整数（全探索用）
genSmallPos :: Gen Int
genSmallPos = choose (1, 15)

-- | AtCoder典型の制約
gen1e5 :: Gen Int
gen1e5 = choose (1, 100000)

gen1e9 :: Gen Int
gen1e9 = choose (1, 1000000000)

gen1e18 :: Gen Integer
gen1e18 = choose (1, 10 ^ 18)

--------------------------------------------------------------------------------
-- リスト・配列ジェネレータ
--------------------------------------------------------------------------------

-- | 長さNのリスト（要素は1〜M）
genList :: Int -> Int -> Gen [Int]
genList n m = vectorOf n $ choose (1, m)

-- | 小さいリスト（全探索可能なサイズ）
genSmallList :: Gen [Int]
genSmallList = do
  n <- choose (0, 12)
  vectorOf n $ choose (1, 100)

-- | 非空リスト
genNonEmptyList :: Gen [Int]
genNonEmptyList = do
  n <- choose (1, 15)
  vectorOf n $ choose (1, 100)

-- | ソート済みリスト
genSortedList :: Int -> Gen [Int]
genSortedList n = sort <$> vectorOf n (choose (1, 1000))

-- | 重複なしリスト
genDistinctList :: Int -> Int -> Gen [Int]
genDistinctList n m = take n . nub <$> infiniteListOf (choose (1, m))

--------------------------------------------------------------------------------
-- 順列・組み合わせ
--------------------------------------------------------------------------------

-- | 1〜Nの順列
genPermutation :: Int -> Gen [Int]
genPermutation n = shuffle [1 .. n]

-- | リストのシャッフル
shuffleList :: [a] -> Gen [a]
shuffleList [] = return []
shuffleList xs = do
  i <- choose (0, length xs - 1)
  let (left, x : right) = splitAt i xs
  rest <- shuffleList (left ++ right)
  return (x : rest)

-- | 部分集合（ビット全探索用）
genSubset :: [a] -> Gen [a]
genSubset xs = do
  mask <- vectorOf (length xs) (elements [True, False])
  return [x | (x, True) <- zip xs mask]

--------------------------------------------------------------------------------
-- ペア・タプル
--------------------------------------------------------------------------------

-- | (A, B) で A <= B
genOrderedPair :: Int -> Int -> Gen (Int, Int)
genOrderedPair lo hi = do
  a <- choose (lo, hi)
  b <- choose (a, hi)
  return (a, b)

-- | (A, B) で A < B（狭義）
genStrictOrderedPair :: Int -> Int -> Gen (Int, Int)
genStrictOrderedPair lo hi = do
  a <- choose (lo, hi - 1)
  b <- choose (a + 1, hi)
  return (a, b)

-- | 重み付きペア [(value, weight)]
genWeightedList :: Int -> Gen [(Int, Int)]
genWeightedList n = vectorOf n $ do
  v <- choose (1, 1000)
  w <- choose (1, 1000)
  return (v, w)

--------------------------------------------------------------------------------
-- 区間
--------------------------------------------------------------------------------

-- | 区間 [L, R] のリスト
genIntervals :: Int -> Int -> Int -> Gen [(Int, Int)]
genIntervals n lo hi = vectorOf n $ genOrderedPair lo hi

-- | 重ならない区間（区間スケジューリング用）
genNonOverlappingIntervals :: Int -> Gen [(Int, Int)]
genNonOverlappingIntervals n = do
  points <- genSortedList (2 * n)
  let pairs = zip (everyOther points) (everyOther $ tail points)
  return pairs
  where
    everyOther [] = []
    everyOther [x] = [x]
    everyOther (x : _ : xs) = x : everyOther xs

--------------------------------------------------------------------------------
-- グラフ
--------------------------------------------------------------------------------

-- | 木（N頂点、N-1辺）
genTree :: Int -> Gen [(Int, Int)]
genTree n = do
  edges <-
    mapM
      ( \i -> do
          j <- choose (1, i - 1)
          return (j, i)
      )
      [2 .. n]
  shuffleList edges

-- | 重み付き木
genWeightedTree :: Int -> Int -> Gen [(Int, Int, Int)]
genWeightedTree n maxW = do
  edges <- genTree n
  mapM
    ( \(u, v) -> do
        w <- choose (1, maxW)
        return (u, v, w)
    )
    edges

-- | 連結グラフ（木 + 追加辺）
genConnectedGraph :: Int -> Int -> Gen [(Int, Int)]
genConnectedGraph n extraEdges = do
  tree <- genTree n
  extras <- vectorOf extraEdges $ do
    u <- choose (1, n)
    v <- choose (1, n) `suchThat` (/= u)
    return (min u v, max u v)
  return $ tree ++ nubOrd extras

-- | DAG（有向非巡回グラフ）
genDAG :: Int -> Int -> Gen [(Int, Int)]
genDAG n m = do
  edges <- vectorOf m $ genStrictOrderedPair 1 n
  return $ nubOrd edges

--------------------------------------------------------------------------------
-- グリッド
--------------------------------------------------------------------------------

-- | H×Wのグリッド（'.' と '#'）
genGrid :: Int -> Int -> Gen [[Char]]
genGrid h w = vectorOf h $ vectorOf w $ elements ".#"

-- | H×Wのグリッド（密度指定）
genGridWithDensity :: Int -> Int -> Int -> Gen [[Char]]
genGridWithDensity h w wallPercent =
  vectorOf h $
    vectorOf w $
      frequency [(100 - wallPercent, return '.'), (wallPercent, return '#')]

-- | スタートとゴールがある迷路
genMaze :: Int -> Int -> Gen ([[Char]], (Int, Int), (Int, Int))
genMaze h w = do
  grid <- genGridWithDensity h w 30
  sr <- choose (0, h - 1)
  sc <- choose (0, w - 1)
  gr <- choose (0, h - 1)
  gc <- choose (0, w - 1)
  let grid' = setCell sr sc '.' $ setCell gr gc '.' grid
  return (grid', (sr, sc), (gr, gc))
  where
    setCell r c v g =
      take r g ++ [take c (g !! r) ++ [v] ++ drop (c + 1) (g !! r)] ++ drop (r + 1) g

--------------------------------------------------------------------------------
-- 文字列
--------------------------------------------------------------------------------

-- | 小文字英字の文字列
genLowerStr :: Int -> Gen String
genLowerStr n = vectorOf n $ elements ['a' .. 'z']

-- | 大文字英字の文字列
genUpperStr :: Int -> Gen String
genUpperStr n = vectorOf n $ elements ['A' .. 'Z']

-- | 数字文字列
genDigitStr :: Int -> Gen String
genDigitStr n = vectorOf n $ elements ['0' .. '9']

-- | 括弧列（バランスしているとは限らない）
genParenStr :: Int -> Gen String
genParenStr n = vectorOf n $ elements "()"

-- | バランスした括弧列
genBalancedParen :: Int -> Gen String
genBalancedParen 0 = return ""
genBalancedParen n = do
  k <- choose (1, n)
  inner <- genBalancedParen (k - 1)
  rest <- genBalancedParen (n - k)
  return $ "(" ++ inner ++ ")" ++ rest

--------------------------------------------------------------------------------
-- ユーティリティ
--------------------------------------------------------------------------------

-- | 条件付き生成（タイムアウト防止のため回数制限）
suchThatMaybe' :: Int -> Gen a -> (a -> Bool) -> Gen (Maybe a)
suchThatMaybe' 0 _ _ = return Nothing
suchThatMaybe' n gen p = do
  x <- gen
  if p x then return (Just x) else suchThatMaybe' (n - 1) gen p

-- | 小さいサイズで実行
withSmallSize :: Gen a -> Gen a
withSmallSize = scale (`div` 4)

-- | デバッグ用：失敗時に入力表示
testWithShow :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
testWithShow gen prop = forAll gen $ \x -> whenFail (print x) (prop x)
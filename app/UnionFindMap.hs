{-- 純粋関数型実装（Map ベース）--}
type UFTree a = M.Map a a

-- 空の Union-Find 木を作成
empty :: UFTree a
empty = M.empty

-- 要素が存在しない場合は自分自身を親として追加
ensure :: Ord a => a -> UFTree a -> UFTree a
ensure x tree = M.insertWith (\_ old -> old) x x tree

-- 要素の根を見つける（経路圧縮付き）
find :: Ord a => a -> UFTree a -> (a, UFTree a)
find x tree =
  let tree' = ensure x tree
      parent = tree' M.! x
   in if parent == x
        then (x, tree')
        else
          let (root, newTree) = find parent tree'
           in (root, M.insert x root newTree)

-- 2つの要素を同じグループに統合
unite :: Ord a => a -> a -> UFTree a -> UFTree a
unite x y tree =
  let (rootX, tree1) = find x tree
      (rootY, tree2) = find y tree1
   in if rootX == rootY
        then tree2
        else M.insert rootX rootY tree2

-- 2つの要素が同じグループに属しているか確認
sameUF :: Ord a => a -> a -> UFTree a -> Bool
sameUF x y tree =
  let (rootX, tree1) = find x tree
      (rootY, _) = find y tree1
   in rootX == rootY

main :: IO ()
main = do
  putStrLn "Union-Find の純粋関数型実装の実行例"

  -- 空のUnion-Find木から開始
  let initialTree = empty :: UFTree Int

  -- 一連の操作を実行
  putStrLn "\n-- 要素を連結 --"
  let tree1 = unite 1 2 initialTree
  let tree2 = unite 2 3 tree1
  let tree3 = unite 4 5 tree2
  let tree4 = unite 6 7 tree3
  let tree5 = unite 5 6 tree4

  -- 各連結操作後のツリーの状態を表示
  putStrLn $ "1と2を連結: " ++ show tree1
  putStrLn $ "2と3を連結: " ++ show tree2
  putStrLn $ "4と5を連結: " ++ show tree3
  putStrLn $ "6と7を連結: " ++ show tree4
  putStrLn $ "5と6を連結: " ++ show tree5

  -- 要素のルートを探索
  putStrLn "\n-- 要素の根を探索 --"
  let (root1, _) = find 1 tree5
  let (root3, _) = find 3 tree5
  let (root4, _) = find 4 tree5
  let (root7, _) = find 7 tree5

  putStrLn $ "要素1の根: " ++ show root1
  putStrLn $ "要素3の根: " ++ show root3
  putStrLn $ "要素4の根: " ++ show root4
  putStrLn $ "要素7の根: " ++ show root7

  -- 接続状態の確認
  putStrLn "\n-- 接続状態の確認 --"
  putStrLn $ "1と3は接続されているか: " ++ show (connected 1 3 tree5)
  putStrLn $ "1と4は接続されているか: " ++ show (connected 1 4 tree5)
  putStrLn $ "4と7は接続されているか: " ++ show (connected 4 7 tree5)
  putStrLn $ "1と7は接続されているか: " ++ show (connected 1 7 tree5)

  -- 経路圧縮の効果を確認
  putStrLn "\n-- 経路圧縮の効果 --"
  let (_, compressedTree) = find 7 tree5
  let (_, compressedTree2) = find 1 compressedTree

  putStrLn "経路圧縮前のツリー:"
  putStrLn $ show tree5
  putStrLn "経路圧縮後のツリー:"
  putStrLn $ show compressedTree2

  -- 新しい連結を追加
  putStrLn "\n-- 新しい連結を追加 --"
  let finalTree = unite 1 4 compressedTree2
  putStrLn $ "1と4を連結: " ++ show finalTree

  -- 最終的な接続状態
  putStrLn "\n-- 最終的な接続状態 --"
  putStrLn $ "1と7は接続されているか: " ++ show (connected 1 7 finalTree)
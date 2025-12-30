{-# LANGUAGE TypeApplications #-}

module IntMultiSet where

import Data.Bool (bool)
import qualified Data.IntMap as IM
import Data.List

data IntMultiSet = IntMultiSet
  { sizeMS :: !Int, -- 集合に含まれる値の個数 O(1)
    distinctSizeMS :: !Int, -- 集合に含まれる値の種類数 O(1)
    mapMS :: IM.IntMap Int
  }
  deriving (Eq)

instance Show IntMultiSet where
  show = show . toListMS

fromListMS :: [Int] -> IntMultiSet
fromListMS = foldl' (flip insertMS) emptyMS

toListMS :: IntMultiSet -> [Int]
toListMS IntMultiSet {mapMS = m} = concatMap @[] (\(k, v) -> replicate v k) (IM.toList m)

emptyMS :: IntMultiSet
emptyMS = IntMultiSet 0 0 IM.empty

singletonMS :: Int -> IntMultiSet
singletonMS x = insertMS x emptyMS

insertMS :: Int -> IntMultiSet -> IntMultiSet
insertMS x (IntMultiSet size dSize m)
  | IM.member x m = IntMultiSet (size + 1) dSize m'
  | otherwise = IntMultiSet (size + 1) (dSize + 1) m'
  where
    m' = IM.insertWith (+) x 1 m

-- x を n 個追加する
-- >>> insertNMS 5 2 (fromListMS [0])
-- [0,2,2,2,2,2]
insertNMS :: Int -> Int -> IntMultiSet -> IntMultiSet
insertNMS n x (IntMultiSet size dSize m)
  | IM.member x m = IntMultiSet (size + n) dSize m'
  | otherwise = IntMultiSet (size + n) (dSize + 1) m'
  where
    m' = IM.insertWith @Int (+) x n m

deleteMS :: Int -> IntMultiSet -> IntMultiSet
deleteMS x s@(IntMultiSet size dSize m)
  | IM.member x m =
      let m' = IM.update @Int (\k -> let k' = k - 1 in bool Nothing (Just k') $ k' > 0) x m
       in if IM.member x m'
            then IntMultiSet (size - 1) dSize m'
            else IntMultiSet (size - 1) (dSize - 1) m'
  | otherwise = s

-- x を n 個削除する
-- 存在する個数以上を指定した場合は削除できる分だけ削除
-- delete 2 100 で 100 を 2 個削除
-- >>> deleteNMS 1 2 (fromListMS [0, 2, 2])
-- [0,2]
deleteNMS :: Int -> Int -> IntMultiSet -> IntMultiSet
deleteNMS n x s@(IntMultiSet size dSize m)
  | IM.member x m = do
      let remain = m IM.! x
          m' = IM.update @Int (\k -> let k' = k - n in bool Nothing (Just k') $ k' > 0) x m
       in if IM.member x m'
            then IntMultiSet (size - n) dSize m'
            else IntMultiSet (size - remain) (dSize - 1) m'
  | otherwise = s

deleteMinMS :: IntMultiSet -> IntMultiSet
deleteMinMS s = deleteMS (findMinMS s) s

deleteMaxMS :: IntMultiSet -> IntMultiSet
deleteMaxMS s = deleteMS (findMaxMS s) s

deleteFindMinMS :: IntMultiSet -> (Int, IntMultiSet)
deleteFindMinMS s = (findMinMS s, deleteMinMS s)

deleteFindMaxMS :: IntMultiSet -> (Int, IntMultiSet)
deleteFindMaxMS s = (findMaxMS s, deleteMaxMS s)

deleteGEViewMS :: Int -> IntMultiSet -> ([(Int, Int)], IntMultiSet)
deleteGEViewMS x s0 = loop s0 []
  where
    loop s acc = case lookupGEMS x s of
      Just v -> do
        let cnt = countMS v s
            s' = deleteNMS cnt v s
        loop s' ((v, cnt) : acc)
      Nothing -> (acc, s)

unionWithMS :: (Int -> Int -> Int) -> IntMultiSet -> IntMultiSet -> IntMultiSet
unionWithMS f (IntMultiSet _ _ a) (IntMultiSet _ _ b) = IntMultiSet (IM.foldl' (+) 0 ab) (IM.size ab) ab
  where
    ab = IM.unionWith f a b

unionMS :: IntMultiSet -> IntMultiSet -> IntMultiSet
unionMS = unionWithMS (+)
{-# INLINE unionMS #-}

differenceWithMS :: (Int -> Int -> Maybe Int) -> IntMultiSet -> IntMultiSet -> IntMultiSet
differenceWithMS f (IntMultiSet _ _ a) (IntMultiSet _ _ b) = IntMultiSet (IM.foldl' (+) 0 c) (IM.size c) c
  where
    c = IM.differenceWith f a b

findMinMS :: IntMultiSet -> Int
findMinMS IntMultiSet {mapMS = m} = (fst . IM.findMin) m

findMaxMS :: IntMultiSet -> Int
findMaxMS IntMultiSet {mapMS = m} = (fst . IM.findMax) m

memberMS :: Int -> IntMultiSet -> Bool
memberMS x IntMultiSet {mapMS = m} = IM.member x m

notMemberMS :: Int -> IntMultiSet -> Bool
notMemberMS x IntMultiSet {mapMS = m} = IM.notMember x m

countMS :: Int -> IntMultiSet -> Int
countMS x IntMultiSet {mapMS = m} = IM.findWithDefault 0 x m

lookupLTMS :: Int -> IntMultiSet -> Maybe Int
lookupLTMS x IntMultiSet {mapMS = m} = (fmap fst . IM.lookupLT x) m

lookupGTMS :: Int -> IntMultiSet -> Maybe Int
lookupGTMS x IntMultiSet {mapMS = m} = (fmap fst . IM.lookupGT x) m

lookupLEMS :: Int -> IntMultiSet -> Maybe Int
lookupLEMS x IntMultiSet {mapMS = m} = (fmap fst . IM.lookupLE x) m

lookupGEMS :: Int -> IntMultiSet -> Maybe Int
lookupGEMS x IntMultiSet {mapMS = m} = (fmap fst . IM.lookupGE x) m

elemsMS :: IntMultiSet -> [Int]
elemsMS IntMultiSet {mapMS = m} = IM.elems m

nullMS :: IntMultiSet -> Bool
nullMS IntMultiSet {mapMS = m} = IM.null m

-- x 以下の値を k 個取得する。k 個ない場合は Nothing
-- >>> topKLE 3 3 (fromListMS [0, 0, 2, 2, 5])
-- Just [2,2,0]
topKLE :: Int -> Int -> IntMultiSet -> Maybe [Int]
topKLE k x set = aux [] x
  where
    aux xs x_ = do
      v <- lookupLEMS x_ set

      let i = countMS v set
          l = length xs

          -- ここで i 全部展開する必要はない
          ys = xs ++ replicate (min (k - l) i) v

      if length ys < k
        then aux ys (v - 1)
        else return $ take k ys

-- x 以上の値を k 個取得する。k 個ない場合は Nothing
-- >>> topKGE 3 1 (fromListMS [0, 0, 2, 2, 5])
-- Just [2,2,5]
topKGE :: Int -> Int -> IntMultiSet -> Maybe [Int]
topKGE k x set = aux [] x
  where
    aux xs x_ = do
      v <- lookupGEMS x_ set

      let i = countMS v set
          l = length xs
          ys = xs ++ replicate (min (k - l) i) v

      if length ys < k
        then aux ys (v + 1)
        else return $ take k ys

-- x 以上の値で k 番目の値を取得する
-- >>> findKthGE 1 4 (fromListMS [0, 0, 2, 2, 5])
-- Just 5
findKthGE :: Int -> Int -> IntMultiSet -> Maybe Int
findKthGE x k set = aux 0 x
  where
    aux cnt x_ = do
      v <- lookupGEMS x_ set

      let i = countMS v set
          cnt' = cnt + min (k - cnt) i

      if cnt' < k
        then aux cnt' (v + 1)
        else return v

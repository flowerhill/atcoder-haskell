module LibSpec (spec) where

import Lib (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "配列の合計を計算" $ do
      solve 3 [1, 2, 3] `shouldBe` 6

    it "空配列は0" $ do
      solve 0 [] `shouldBe` 0
module BarTest where

import Test.Tasty.Hspec (hspec, describe, it, shouldBe)

case_headIsWorking = hspec $
  describe "Does Prelude.head still work?" $
    it "returns the first element of a list" $
      head [23..] `shouldBe` (23 :: Int)

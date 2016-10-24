module BarTest where

import Test.Tasty.Discover (hspec, describe, it, shouldBe)

case_headIsWorking = hspec $
  describe "Check if Prelude.head 'still has it'" $
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)

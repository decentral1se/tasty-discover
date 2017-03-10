module BarTest where

import Test.Tasty.Hspec (hspec, describe, it, shouldBe)

spec_headIsWorking =
  describe "Check if Prelude.head 'still has it'" $
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)

spec_tailIsWorking =
  describe "Check if Prelude.tail 'still has it'" $
    it "returns the tail of a list" $
      tail [1..3] `shouldBe` [(2::Int)..3]

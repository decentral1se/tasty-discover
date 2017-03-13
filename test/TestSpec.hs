module TestSpec where

import Test.Tasty.Hspec

spec_Prelude :: Spec
spec_Prelude =
  describe "Prelude.head" $
  it "returns the first element of a list" $
  head [23 ..] `shouldBe` (23 :: Int)

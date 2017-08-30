{-# LANGUAGE ScopedTypeVariables #-}

module DiscoverTest where

import           Data.List
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

unit_listCompare :: IO ()
unit_listCompare = [1 :: Int, 2, 3] `compare` [1,2] @?= GT

prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)

spec_prelude :: Spec
spec_prelude =
  describe "Prelude.head" $
  it "returns the first element of a list" $
  head [23 ..] `shouldBe` (23 :: Int)

test_addition :: TestTree
test_addition = testProperty "Addition commutes" $ \(a :: Int) (b :: Int) -> a + b == b + a

test_multiplication :: [TestTree]
test_multiplication =
  [ testProperty "Multiplication commutes" $ \(a :: Int) (b :: Int) -> a * b == b * a
  , testProperty "One is identity" $ \(a :: Int) -> a == a
  ]

test_generateTree :: IO TestTree
test_generateTree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()

test_generateTrees :: IO [TestTree]
test_generateTrees = do
  inputs <- pure ["First input", "Second input"]
  pure $ map (\s -> testCase s $ pure ()) inputs

{-# ANN hprop_reverse "HLint: ignore Avoid reverse" #-}
hprop_reverse :: H.Property
hprop_reverse = H.property $ do
  xs <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  reverse (reverse xs) H.=== xs

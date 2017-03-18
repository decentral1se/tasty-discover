{-# LANGUAGE ScopedTypeVariables #-}

module DiscoverTest where

import           Data.List
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

case_listCompare :: IO ()
case_listCompare = [1 :: Int, 2, 3] `compare` [1,2] @?= GT

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

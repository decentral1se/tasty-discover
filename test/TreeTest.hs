{-# LANGUAGE ScopedTypeVariables #-}
module TreeTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

test_Addition :: TestTree
test_Addition = testProperty "Addition commutes" $ \(a :: Int) (b :: Int) -> a + b == b + a

test_Multiplication :: [TestTree]
test_Multiplication =
  [ testProperty "Multiplication commutes" $ \(a :: Int) (b :: Int) -> a * b == b * a
  , testProperty "One is identity" $ \(a :: Int) -> a == a
  ]

test_Generate_Tree :: IO TestTree
test_Generate_Tree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()

test_Generate_Trees :: IO [TestTree]
test_Generate_Trees = do
  inputs <- pure ["First input", "Second input"]
  pure $ map (\s -> testCase s $ pure ()) inputs

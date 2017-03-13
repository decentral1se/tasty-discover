module CaseTest where

import Test.Tasty.HUnit

case_List_comparison_with_different_length :: IO ()
case_List_comparison_with_different_length = [1 :: Int, 2, 3] `compare` [1,2] @?= GT

module SomeTest (props) where

import Test.Tasty
import Test.Tasty.QuickCheck as QuickCheck

props :: TestTree
props = testGroup "" [QuickCheck.testProperty "" $ \n -> (n :: Int) == n]

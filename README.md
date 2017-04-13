[![Build Status](https://travis-ci.org/lwm/tasty-discover.svg?branch=master)](https://travis-ci.org/lwm/tasty-discover)
[![Hackage Status](https://img.shields.io/badge/Hackage-2.0.3-brightgreen.svg)](http://hackage.haskell.org/package/tasty-discover)
[![Stackage Status](https://img.shields.io/badge/Stackage-2.0.3-brightgreen.svg)](https://www.stackage.org/package/tasty-discover/)
[![GitHub license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://raw.githubusercontent.com/lwm/tasty-discover/master/LICENSE)

# tasty-discover

Automatic test discovery and runner for the [tasty framework].

[tasty framework]: https://github.com/feuerbach/tasty

# Getting Started

![Usage GIF](http://i.imgur.com/gpdHc6x.gif)

5 steps to tasty test discovery satori:
  - Create a `Tasty.hs` in the `hs-source-dirs` of your test suite.
  - Set your test suite `main-is` to the `Tasty.hs`.
  - Create test modules in files with suffix `*Test.hs` or `*Spec.hs`.
  - Write your tests with the following prefixes:
    - `prop_`: [QuickCheck](http://hackage.haskell.org/package/tasty-quickcheck) properties.
    - `scprop_`: [SmallCheck](http://hackage.haskell.org/package/tasty-smallcheck) properties.
    - `case_`: [HUnit](http://hackage.haskell.org/package/tasty-hunit) test cases.
    - `spec_`: [Hspec](http://hackage.haskell.org/package/tasty-hspec) specifications.
    - `test_`: [Tasty](http://hackage.haskell.org/package/tasty) TestTrees.

# Examples

``` haskell
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

-- HUnit test case
case_listCompare :: IO ()
case_listCompare = [1, 2, 3] `compare` [1,2] @?= GT

-- QuickCheck property
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

-- SmallSheck property
scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)

-- Hspec specification
spec_prelude :: Spec
spec_prelude = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

-- Tasty TestTree
test_multiplication :: [TestTree]
test_multiplication = [testProperty "One is identity" $ \(a :: Int) -> a * 1 == a]

-- Tasty IO TestTree
test_generateTree :: IO TestTree
test_generateTree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()

-- Tasty IO [TestTree]
test_generateTrees :: IO [TestTree]
test_generateTrees = do
  inputs <- pure ["First input", "Second input"]
  pure $ map (\s -> testCase s $ pure ()) inputs
```

# Configuration

Pass configuration options within your `Tasty.hs` like so:

``` haskell
{-#
 OPTIONS_GHC -F -pgmF tasty-discover
 -optF <OPTION>
 -optF <OPTION>
#-}
```

## No Arguments
Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --debug #-}`

  - `--no-module-suffix`: Collect all test modules, regardless of module suffix.
  - `--debug`: Output the contents of the generated module while testing.

## With Arguments
Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --moduleSuffix=FooBar #-}`

  - `--module-suffix`: Which test module suffix you wish to have discovered.
  - `--generated-module`: The name of the generated test module.
  - `--ignore-module`: Which test modules to ignore from discovery.
  - `--ingredient`: Tasty ingredients to add to your test runner.

# Change Log
See the [Change log] for the latest changes.

[Change log]: https://github.com/lwm/tasty-discover/blob/master/CHANGELOG.md

# Contributing
All contributions welcome!

# Acknowledgements
Thanks to [hspec-discover] and [tasty-auto] for making this possible.

[hspec-discover]: https://hspec.github.io/hspec-discover.html
[tasty-auto]: https://github.com/minad/tasty-auto

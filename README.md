[![Build Status](https://travis-ci.org/lwm/tasty-discover.svg?branch=master)](https://travis-ci.org/lwm/tasty-discover)
[![Hackage Status](https://img.shields.io/hackage/v/tasty-discover.svg)](http://hackage.haskell.org/package/tasty-discover)
[![tasty-discover](http://stackage.org/package/tasty-discover/badge/nightly)](http://stackage.org/nightly/package/tasty-discover)
[![GitHub license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://raw.githubusercontent.com/lwm/tasty-discover/master/LICENSE)

# tasty-discover

Automatic test discovery and runner for the [tasty framework].

[tasty framework]: https://github.com/feuerbach/tasty

# Getting Started

There's 3 simple steps:

  1. Create a test driver file
  2. Mark it as the main-is in your test suite
  3. Name your tests with correct prefixes

## Create Test Driver File

You can name this file anything you want but it must contain
the correct preprocessor definition for tasty-discover to run
and detect your configuration. It should be in the top level
of the directory with all your tests.

Here's an example:

```
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```

## Configure Cabal Test Suite

In order for Cabal/Stack to know where your tests are, you'll need to configure
the main-is option of your test-suite to point to that file. In the following
example, the test driver file is called Test.hs:

```
test-suite test
  main-is: Test.hs
  hs-source-dirs: tests
  build-depends: base
```

# Write Tests

Create modules with file suffix Test.hs and correctly prefix your
tests with the name that corresponds to the testing library:

  - **prop_**: [QuickCheck](http://hackage.haskell.org/package/tasty-quickcheck) properties.
  - **scprop_**: [SmallCheck](http://hackage.haskell.org/package/tasty-smallcheck) properties.
  - **hprop_**: [Hedgehog](http://hackage.haskell.org/package/tasty-hedgehog) properties.
  - **unit_**: [HUnit](http://hackage.haskell.org/package/tasty-hunit) test cases.
  - **spec_**: [Hspec](http://hackage.haskell.org/package/tasty-hspec) specifications.
  - **test_**: [Tasty](http://hackage.haskell.org/package/tasty) TestTrees.

Here's an example test module:

``` haskell
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleTest where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

-- HUnit test case
unit_listCompare :: IO ()
unit_listCompare = [1, 2, 3] `compare` [1,2] @?= GT

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

# Customise Discovery

You configure tasty-discover by passing options to the test driver file.

## No Arguments

Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --debug #-}`

  - **--no-module-suffix**: Collect all test modules, regardless of module suffix.
  - **--debug**: Output the contents of the generated module while testing.
  - **--tree-display**: Display the test output results hierarchically.

## With Arguments

Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --module-suffix=FooBar #-}`

  - **--module-suffix**: Which test module suffix you wish to have discovered.
  - **--generated-module**: The name of the generated test module.
  - **--ignore-module**: Which test modules to ignore from discovery.
  - **--ingredient**: Tasty ingredients to add to your test runner.

# Example Project

See the [testing for this package] for a fully configured example.

[testing for this package]: https://github.com/lwm/tasty-discover/tree/master/test

# Change Log

Please see the [CHANGELOG.md] for the latest changes.

[CHANGELOG.md]: https://github.com/lwm/tasty-discover/blob/master/CHANGELOG.md

# Deprecation Policy

If a breaking change is implemented, you'll see a major version increase, an
entry in the [change log] and a compile time error with a deprecation warning
and clear instructions on how to upgrade. Please do complain if we're doing
this too much.

[change log]: https://github.com/lwm/tasty-discover/blob/master/CHANGELOG.md

# Contributing

All contributions welcome!

# Acknowledgements

Thanks to [hspec-discover] and [tasty-auto] for making this possible.

[hspec-discover]: https://hspec.github.io/hspec-discover.html
[tasty-auto]: https://github.com/minad/tasty-auto

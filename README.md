[![Build Status](https://travis-ci.org/lwm/tasty-discover.svg?branch=master)](https://travis-ci.org/lwm/tasty-discover)
[![tasty-discover-nightly](http://stackage.org/package/tasty-discover/badge/nightly)](http://stackage.org/nightly/package/tasty-discover)
[![tasty-discover-lts](http://stackage.org/package/tasty-discover/badge/lts)](http://stackage.org/lts/package/tasty-discover)
[![Hackage Status](https://img.shields.io/hackage/v/tasty-discover.svg)](http://hackage.haskell.org/package/tasty-discover)
[![GitHub license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://raw.githubusercontent.com/lwm/tasty-discover/master/LICENSE)

# tasty-discover

Automatic test discovery and runner for the [tasty framework].

[tasty framework]: https://github.com/feuerbach/tasty

# Getting Started

There's 4 simple steps:

  1. [Create a test driver file in the test directory](#create-test-driver-file)
  2. [Mark the driver file as the `main-is` in the test suite](#configure-cabal-test-suite)
  3. [Mark tests with the correct prefixes](#write-tests)
  4. [Customise test discovery as needed](#customise-discovery)

Check out the [example project](#example-project) to get moving quickly.

## Create Test Driver File

You can name this file anything you want but it must contain the correct
preprocessor definition for tasty-discover to run and also, to detect the
configuration. It should be in the top level of the test directory.

For example (in `test/Driver.hs`):

```
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```

## Configure Cabal Test Suite

In order for Cabal/Stack to know where the tests are, you'll need to configure
the main-is option of your test-suite to point to that file. In the following
example, the test driver file is called Driver.hs:

```
test-suite test
  main-is: Driver.hs
  hs-source-dirs: tests
  build-depends: base
```

If you use [hpack], that might look like:

[hpack]: https://github.com/sol/hpack

``` yaml
tests:
  test:
    main: "Driver.hs"
    source-dirs: "test"
    dependencies:
    - "base"
```

# Write Tests

Create test modules and correctly prefix the tests with the name that
corresponds to the testing library you wish to run the test with:

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

  - **--debug**: Output the contents of the generated module while testing.
  - **--tree-display**: Display the test output results hierarchically.

## With Arguments

Example: `{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --modules="*CustomTest.hs" #-}`

  - **--modules**: Which test modules to discover (with glob pattern).
  - **--ignores**: Which test modules to ignore (with glob pattern).
  - **--generated-module**: The name of the generated test module.
  - **--ingredient**: Tasty ingredients to add to your test runner.

It is also possible to override tasty arguments with `-optF`:

``` bash
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --hide-successes #-}
```

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

# Maintenance

If you're interested in helping maintain this package, please let [@lwm] know!

[@lwm]: https://github.com/lwm

# Acknowledgements

Thanks to [hspec-discover] and [tasty-auto] for making this possible.

[hspec-discover]: https://hspec.github.io/hspec-discover.html
[tasty-auto]: https://github.com/minad/tasty-auto

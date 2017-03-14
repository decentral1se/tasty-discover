[![Build Status](https://travis-ci.org/lwm/tasty-discover.svg?branch=master)](https://travis-ci.org/lwm/tasty-discover)
[![Hackage Status](https://img.shields.io/badge/Hackage-1.0.1-brightgreen.svg)](http://hackage.haskell.org/package/tasty-discover)
[![Stackage Status](https://img.shields.io/badge/Stackage-1.0.1-brightgreen.svg)](https://www.stackage.org/package/tasty-discover/)
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

# Configuration

Pass configuration options within your `Tasty.hs` like so:

```haskell
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF [OPTIONS] #-}
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

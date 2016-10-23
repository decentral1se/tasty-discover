[![Build Status](https://travis-ci.org/lwm/tasty-discover.svg?branch=master)](https://travis-ci.org/lwm/tasty-discover)
[![codecov](https://codecov.io/gh/lwm/tasty-discover/branch/master/graph/badge.svg)](https://codecov.io/gh/lwm/tasty-discover)
![GHC Versions](https://img.shields.io/badge/GHC-7.10.2-brightgreen.svg)
![License](https://img.shields.io/badge/license-GPLv3-brightgreen.svg)

# tasty-discover
Automatic test discovery and runner for the [tasty framework][tasty-framework].

### Table of Contents

- [Status](#status)
- [Examples](#examples)
- [Project integration](#project-integration)
- [Getting started](#getting-started)
- [Stack template](#stack-template)
- [Configuration](#configuration)
    - [Define your own test module suffix](#define-your-own-test-module-suffix)
    - [Omit test module suffix](#omit-test-module-suffix)
- [Contributing](#contributing)
    - [Testing](#testing)
- [Other Haskellers made this possible](#other-haskellers-made-this-possible)
- [Related documentation](#related-documentation)

### Status
So far, this package is only a hackage candidate. I'd like to see that change
shortly but for now, to use this package in your projects, you'll need to get
it from Github. See the [project integration][project-integration] for more
details.

### Examples
  - Basic example: [example/][example-project]
  - Unit tests: [test/][test-folder] folder.
  - Integration tests: [integration-test/][integration-tests]

### Project integration
If you are using [stack][stack-haskell], you can add the following to your `stack.yaml`:

```
packages:
- '.'
- location:
    git: "https://github.com/lwm/tasty-discover.git"
    commit: "HEAD"
```

Note. `tasty-discover` relies on `tasty-th-0.1.4`. You may need to explicitly set this in your `stack.yaml`:

```
extra-deps:
- "tasty-th-0.1.4"
```

### Getting started
`tasty-discover` expects the following conventions:

  - Cabal test suite `main-is` must point to a file with the necessary GHC preprocessor line. ([example](https://github.com/lwm/tasty-discover/blob/master/example/test/Tasty.hs))
  - Test files ending with `Test.hs` (this is now configurable, see [configuration][configuration])
  - Test cases starting with either `prop_`, `case_` (related to [tasty-th usage](https://github.com/bennofs/tasty-th#usage))
    - It is also possible to pass `test_` for test groups

In order to get started, you need to set up your test suite with `Cabal`.
Add the following to your cabal file:

```
test-suite your-test-suite-name
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test  -- your test directory
  main-is:             Tasty.hs  -- your `tasty-discover` pre processor
  other-modules:       FooTest -- your test files
  build-depends:       base, tasty-discover  -- your dependencies
  default-language:    Haskell2010
```

Create a `test` folder, and place a file in it called `Tasty.hs`.
Inside this file, add the following:

```
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```

Create a test file inside `test/`. `tasty-discover` can detect test files in
whatever directory structure you choose (as long as they sit in the same
directory as the `Tasty.hs` file created above or any subdirectory thereof), so
there is no restrictions on directory organization. By default, you must end your
test file names with `Test.hs` for them to be discovered. So, for example, `FooTest.hs`
would be a perfectly acceptable test file name.

`tasty-discover` exports the `tasty-quickcheck` and `tasty-hunit` modules, so
we can simply write the following in `test/FooTest.hs`:

```
module FooTest where

import Test.Tasty.Discover

prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_of_one :: Assertion
case_length_of_one = 1 @=? length [()]
```

Now, we can run our tests with:

```
$ stack test
```

### Stack template
A `tasty-discover` `stack` template is being maintained [here][tasty-discover-template],
which you can start a new project with by running:

```
$ stack new <project-name> tasty-discover
```

This template accepts a number of parameters, which `stack` helpfully tells us, we can:

```
You can provide them in /home/<username>/.stack/config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
```

### Configuration

See the [integration tests][Integration-tests] for more details.

#### Define your own test module suffix
If you would prefer to end your test module file names with something other
than `Test.hs`, you can set the following preprocessor line:

```
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --module-suffix=MySuffix #-}
```

`tasty-discover` will now search for test modules with the `MySuffix.hs` file
ending. If this option is not present, the default is the `Test.hs` suffix.

#### Omit test module suffix
If you would prefer to avoid the test file suffix naming convention, you can
set the following preprocessor line:

```
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --no-module-suffix #-}
```

`tasty-discover` will now search for tests in all files under `hs-source-dirs`
regardless of suffix.

### Contributing
Pull requests are very welcome! Please submit an issue so we can discuss what
you would like to do. You can simply fork this repository, and open a pull request.
Check the issues for things that can be worked on immediately.

#### Testing
`tasty-discover` uses `tasty-discover` to test itself. I'd like to see your
changes tested in the `integration-test/` and/or `test/` folders, where
applicable. If you think the feature is applicable to a wide audience, you
could add it to the `example/` folder as well.

You can run the tests as follows:

```
$ make test             # all tests
$ make unit_test        # only unit tests
$ make integration_test # only integration tests
$ make example_test     # only the example code tests
```

### Other Haskellers made this possible
  - [tasty][tasty-framework]
  - [tasty-th][tasty-th]
  - [hspec][hspec]

### Related documentation
  - [hspec-discover documentation][hspec-discover]
  - [tasty-th documentation][tasty-th-docs]

[issues]: https://github.com/lwm/tasty-discover/issues
[stack-haskell]: https://github.com/commercialhaskell/stack
[tasty-th-docs]: https://github.com/bennofs/tasty-th#usage
[tasty-th]: https://github.com/bennofs/tasty-th
[hspec]: https://github.com/hspec/hspec
[hspec-discover]: https://hspec.github.io/hspec-discover.html
[project-integration]: https://github.com/lwm/tasty-discover#project-integration
[tasty-framework]: https://github.com/feuerbach/tasty
[integration-tests]: https://github.com/lwm/tasty-discover/tree/master/integration-test
[example-project]: https://github.com/lwm/tasty-discover/tree/master/example
[configuration]: https://github.com/lwm/tasty-discover#configuration
[tasty-discover-template]: https://github.com/commercialhaskell/stack-templates/blob/master/tasty-discover.hsfiles
[test-folder]: https://github.com/lwm/tasty-discover/tree/master/test

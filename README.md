# tasty-discover
Automatic test discovery with the Tasty framework. Simply write your tests of
type `Assertion` (with [hunit][hunit]) or `Bool` (with
[quickcheck][quickcheck]) and have `tasty-discover` do everything else for you.
More testing libraries will be added as I get time to do it. I hope this can
become a good Tasty framework citizen which enables Haskellers to write less
test boilerplate.

This package is largely based on the good work of other Haskellers. See the
details [in a section below](https://github.com/lwm/tasty-discover/#standing-on-the-shoulders-of-other-haskellers).

### The rules are simple

  - Cabal test suite `main-is` must point to a file with the necessary GHC preprocessor line. (see below)
  - Test files must end with `Test.hs`
  - Test cases must start with either `prop_`, `case_` or `test_`

### Quick example
If you'd like to get a running example in a few minutes, run the following
commands. Otherwise, read below for a more step by step guide.

Firstly, fork this repository and [install stack][installstack]. Then:

```
$ git clone https://github.com/<your-username>/tasty-discover
$ cd tasty-discover/integration-test
$ stack setup && stack build --test
```

If you're looking for the lovely `tasty` test output, you will have to do the
following:

```
$ cat .stack-work/logs/*-test.log
```

So far, I haven't figured out how to retrieve the test report. I have raised
[the issue][stackissue] with the stack folks. I would openly welcome any
contributions on this topic!

### Walkthrough
In order to get started, you need to set up your test suite with `Cabal`.
You'll only need the minimal boilerplate with `tasty-discover`. Add the
following to your cabal

```
test-suite your-test-suite-name
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test  -- your test directory
  main-is:             Tasty.hs  -- your `tasty-discover` pre processor
  other-modules:       XTest, YTest, Folder.ZTest -- your test files
  build-depends:       base, tasty-discover  -- your dependencies
  default-language:    Haskell2010
```

Note. `tasty-discover` relies on `tasty-th` version `0.1.4`, so you may need to
explicitly set this in your `stack.yaml` like so:

```
extra-deps:
- "tasty-th-0.1.4"
```

Next, create a `test` folder, and place a file in it called `Tasty.hs`.
Inside this file, add the following:

```
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```

Next, let's create a test file. `tasty-discover` can detect test files in
whatever directory structure you choose (as long as they sit under your
`hs-source-dirs`), so there is no restrictions on directory organization.
However, you must end your test file names with `Test.hs`. So, for example,
`EatYourLandlordTest.hs` would be a perfectly acceptable test file name.

`tasty-discover` exports the `tasty-quickcheck` and `tasty-hunit` modules, so
we can simple write the following in `test/EatYourLandlordTest.hs`:

```
module EatYourLandlordTest where

import Test.Tasty.Discover

prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_of_one :: Assertion
case_length_of_one = 1 @=? length [()]
```

Now, we can run our tests back in the project root directory:

```
$ stack test
```

`tasty-discover` then generates the necessary boilerplate to import your test
files and parses your test function names and runs them! Simple! You can see a
minimal test project with `tasty-discover` [located here][minimalsetup].

### Contributing
Pull requests are very welcome! Please submit an issue so we can discuss what
you would like to do. I test any changes within the `integration-test` file
directory, which has its own `.cabal` and `stack.yaml` file. Please add some
sort of test there if you want to add functionality. Also, see below for some
things that could be worked on immediately.

### TODO
  - [ ] Get the Travis CI passing (the tests pass already!)
  - [ ] Get Hspec and Smallcheck under the `tasty-discover` umbrella
  - [ ] Make a full release to Hackage/Stackage (currently candidate)
  - [ ] `stack haddock` works but the documentation isn't appearing on the candidate page?


### Standing on the shoulders of other Haskellers
This project wouldn't have been possible without the awesome work of the
people involved with the following projects:

  - [tasty](https://github.com/feuerbach/tasty)
  - [tasty-th](http://hackage.haskell.org/package/tasty-th)
  - [hspec](https://github.com/hspec/hspec)

Related documentation:
  - [hspec-discover documentation][hspecdiscover]
  - [tasty-th documentation][tastythdocs]


[issues]: https://github.com/lwm/tasty-discover/issues
[stackhaskell]: https://github.com/commercialhaskell/stack
[installstack]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
[stackissue]: https://github.com/commercialhaskell/stack/issues/426#issuecomment-186237534
[tastythdocs]: https://github.com/bennofs/tasty-th#usage
[hspecdiscover]: https://hspec.github.io/hspec-discover.html
[minimalsetup]: https://github.com/lwm/tasty-discover/tree/master/integration-test
[hunit]: https://github.com/hspec/HUnit#readme
[quickcheck]: https://github.com/nick8325/quickcheck

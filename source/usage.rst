Usage
=====

Expected Conventions
--------------------

tasty-discover expects the following conventions in order to discover and run your tests:

* Cabal test suite main-is refers to a preprocessor module.
* Test modules suffixed with Test.hs.
* Test cases prefixed with prop\_, case\_, or grouped with test\_.

  * prop\_ -> A Quickcheck property test
  * case\_ -> A Hspec or Hunit test
  * test\_ -> One or several tests grouped together.

.. note:: Test file suffixes are configurable or not necessary depending on your setup.
          Please refer to `the configuration`_ for more information.

.. _the configuration: TODO

Project Example
---------------

To see it all in action, review the `basic project example`_.

.. _basic project example: https://github.com/lwm/tasty-discover/tree/master/example


Full Example
------------

First, you must set up your test suite with Cabal. Add the following to your cabal file:

.. code-block:: haskell

    test-suite <your-test-suite-name>
      type:                exitcode-stdio-1.0
      hs-source-dirs:      test                 -- test directory
      main-is:             Tasty.hs             -- preprocessor files
      other-modules:       FooTest              -- test modules
      build-depends:       base, tasty-discover -- dependencies
      default-language:    Haskell2010

In your test/ folder, create a file called Tasty.hs with the following contents:

.. code-block:: haskell

    {-# OPTIONS_GHC -F -pgmF tasty-discover #-}

Then create FooTest.hs in test/. By default, you must end your test file names
with Test.hs for them to be discovered. So, for example, FooTest.hs would be a
perfectly acceptable test file name.

.. note:: tasty-discover can detect test modules in whatever directory structure you choose,
          as long as they sit in the same directory as the Tasty.hs file created above or
          any subdirectory thereof.

Then, in test/FooTest.hs, add some tests:


.. code-block:: haskell

    module FooTest where

    import Test.Tasty.Discover

    prop_length_append :: [Int] -> [Int] -> Bool
    prop_length_append as bs = length (as ++ bs) == length as + length bs

    case_length_of_one :: Assertion
    case_length_of_one = 1 @=? length [()]

Finally, run the test suite (using Stack):

.. code-block:: bash

    λ stack test

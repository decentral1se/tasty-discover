Configuration
=============

Define your own test module suffix
----------------------------------

If you would prefer to end your test module file names with something other
than Test.hs, you can set the following preprocessor line:

::

    {-# OPTIONS_GHC -F -pgmF tasty-discover -optF --module-suffix=MySuffix #-}

tasty-discover will now search for test modules with the MySuffix.hs file
ending. If this option is not present, the default is the Test.hs suffix.

Omit test module suffix
-----------------------

If you would prefer to avoid the test file suffix naming convention, you can
set the following preprocessor line:

::

    {-# OPTIONS_GHC -F -pgmF tasty-discover -optF --no-module-suffix #-}

tasty-discover will now search for tests in all files under hs-source-dirs
regardless of suffix.

.. note:: See the `integration tests`_ folder for examples of each configuration in practice.

.. _integration tests: https://github.com/lwm/tasty-discover/tree/master/integration-test

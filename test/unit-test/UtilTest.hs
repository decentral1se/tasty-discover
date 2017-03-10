-- Unit tests for Test.Tasty.Util module.

module UtilTest where

import Test.Tasty.Discover (Assertion, (@?=), defaultConfig, getListOfTests,
                            findTests, fileToTest, getFilesRecursive,
                            isValidModuleChar, isValidModuleName,
                            Config(..), Test(..))

case_getListOfTests :: Assertion
case_getListOfTests = do
  result <- getListOfTests "test/unit-test/tmpdir/" defaultConfig
  result @?= ["case_foo"]

case_getListOfTestsWithSuffix :: Assertion
case_getListOfTestsWithSuffix = do
  let config = Config (Just "DoesntExist") False []
  result <- getListOfTests "test/unit-test/tmpdir/" config
  result @?= []

case_findTests :: Assertion
case_findTests = do
  result <- findTests "test/unit-test/tmpdir/" defaultConfig
  result @?= [Test {testFile="test/unit-test/tmpdir/FooTest.hs", testModule="Foo"}]

case_fileToTest :: Assertion
case_fileToTest = do
  let result = fileToTest "test/unit-test/tmpdir/" defaultConfig "FooTest.hs"
  result @?= Just Test {testFile="test/unit-test/tmpdir/FooTest.hs", testModule="Foo"}

case_getFilesRecursive :: Assertion
case_getFilesRecursive = do
  result <- getFilesRecursive "test/unit-test/tmpdir/"
  result @?= ["FooTest.hs", "README.md"]

case_isValidModuleChar :: Assertion
case_isValidModuleChar = isValidModuleChar 'C' @?= True

case_isValidModuleName :: Assertion
case_isValidModuleName = isValidModuleName "Jim" @?= True

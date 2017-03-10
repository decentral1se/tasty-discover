-- Unit tests to assure `tasty-discover` is discovering tests.

module RunnerTest where

import Test.Tasty.HUnit (Assertion, (@?))
import Test.Tasty.Discover (defaultConfig, getListOfTests)

case_unitTestsDiscovered :: Assertion
case_unitTestsDiscovered = do
  unitTests <- getListOfTests "test" defaultConfig
  (return $ null unitTests :: IO Bool) @? "Couldn't find any unit tests."

case_integrationTestsDiscovered :: Assertion
case_integrationTestsDiscovered = do
  integrationTests <- getListOfTests "integration-test/" defaultConfig
  (return $ null integrationTests :: IO Bool) @? "Couldn't find any integration tests."

case_exampleTestsDiscovered :: Assertion
case_exampleTestsDiscovered = do
  exampleTests <- getListOfTests "example/" defaultConfig
  (return $ null exampleTests :: IO Bool) @? "Couldn't find any example tests."

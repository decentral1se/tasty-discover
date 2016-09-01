module FooTest where

import Test.Tasty.Discover (Assertion, (@?=), TestTree, testCase)

test_allMyTestsGrouped :: [TestTree]
test_allMyTestsGrouped =
    [ testCase "Testing the meaning of life." case_theAnswer
    , testCase "Testing the number of the beast." case_theNumberOfTheBeast
    ]

case_theAnswer :: Assertion
case_theAnswer = 42 @?= (42 :: Integer)

case_theNumberOfTheBeast :: Assertion
case_theNumberOfTheBeast = 666 @?= (666 :: Integer)

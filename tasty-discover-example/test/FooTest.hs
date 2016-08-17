module FooTest where

import Test.Tasty.Discover (Assertion, (@?=))

case_theAnswer :: Assertion
case_theAnswer = 42 @?= (42 :: Integer)

case_theNumberOfTheBeast :: Assertion
case_theNumberOfTheBeast = 666 @?= (666 :: Integer)

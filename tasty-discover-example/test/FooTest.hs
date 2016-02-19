module FooTest where

import Test.Tasty.Discover (Assertion, (@?=))

case_the_answer :: Assertion
case_the_answer = 42 @?= 42

case_the_number_of_the_beast :: Assertion
case_the_number_of_the_beast = 666 @?= 666

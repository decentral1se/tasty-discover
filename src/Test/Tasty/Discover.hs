-- | Automatic test discovery with the Tasty framework. Simply write your tests
-- of type Assertion (with hunit) or Bool (with quickcheck) and have
-- tasty-discover do everything else for you. More testing libraries will be
-- added as I get time to do it. I hope this can become a good Tasty framework
-- citizen which enables Haskellers to write less test boilerplate.
--
-- When a, for example, `stack test` gets called, the preprocessor test
-- file will thread arguments into the `Test.Tasty.Run` module which performs
-- all the boilerplate generation. So, if you wanna hack on `tasty-discover`,
-- please check it out below.
--
-- For more practical usage, please refer to the Github documentation.
-- https://github.com/lwm/tasty-discover

module Test.Tasty.Discover (
    module Test.Tasty
  , module Test.Tasty.TH
  , module Test.Tasty.HUnit
  , module Test.Tasty.QuickCheck
  , module Test.Tasty.Run
) where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Tasty.Run

-- | Automatic test discovery with the Tasty framework.
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

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
import           Test.Tasty.Run

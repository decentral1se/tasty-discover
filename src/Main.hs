-- | The main module which threads the preprocessor arguments
--   into the test generator logic.

module Main (main) where

import           Test.Tasty.Prelude
import           Test.Tasty.Run     (run)

main :: IO ()
main = getArgs >>= run

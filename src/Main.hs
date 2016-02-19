-- | The main module which threads the preprocessor arguments
--   into the test generator logic.

module Main (main) where

import System.Environment  (getArgs)
import Test.Tasty.Run      (run)

main :: IO ()
main = getArgs >>= run

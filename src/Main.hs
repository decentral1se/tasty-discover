-- | Main module and entry point.

module Main where

import System.Environment (getArgs)

import Test.Tasty.Run (run)

-- | Main function. Simply threads preprocessor arguments.
main :: IO ()
main = getArgs >>= run

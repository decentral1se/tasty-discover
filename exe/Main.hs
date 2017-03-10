-- | Main module and entry point.

module Main where

import System.Environment (getArgs)

import Test.Tasty.Discover (run)

-- | Pass pre processor arguments.
main :: IO ()
main = getArgs >>= run

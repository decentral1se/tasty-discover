-- | Main module which passes preprocessor arguments to `Test.Tasty.Run`.

module Main where

import System.Environment (getArgs)

import Test.Tasty.Run (run)

main :: IO ()
main = getArgs >>= run

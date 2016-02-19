module Main (main) where

import System.Environment  (getArgs)
import Test.Tasty.Run      (run)

main :: IO ()
main = getArgs >>= run

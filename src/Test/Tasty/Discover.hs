module Test.Tasty.Discover (main) where

import System.Environment  (getArgs)
import Test.Tasty.Run      (run)

main :: IO ()
main = getArgs >>= run

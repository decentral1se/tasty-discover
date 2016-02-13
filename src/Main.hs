module Main (main) where

import System.Environment  (getArgs)
import Run                 (run)

main :: IO ()
main = getArgs >>= run

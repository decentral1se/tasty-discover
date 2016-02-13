module Main (main) where

import System.Environment

main :: IO ()
main = getArgs >>= print

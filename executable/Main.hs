-- | Main executable module.
module Main where

import Control.Monad (when)
import Test.Tasty.Config (parseConfig)
import Test.Tasty.Discover (findTests, generateTestDriver)
import Test.Tasty.Type (Config(..))
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Main function.
main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    src : _ : dst : opts ->
      case parseConfig name opts of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right config -> do
          tests <- findTests src config
          let output = generateTestDriver "Main" [] src tests
          when (debug config) $ hPutStrLn stderr output
          writeFile dst output
    _ -> do
      hPutStrLn stderr "Usage: tasty-discover src _ dst [OPTION...]"
      exitFailure

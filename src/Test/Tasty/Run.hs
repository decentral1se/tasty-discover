-- | Test discovery and runner boilerplate generator.

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tasty.Run (
  run
, tmpModule
) where

import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Test.Tasty.Parse (parseConfig)
import Test.Tasty.Util (importList, findTests, getListOfTests)
import Test.Tasty.Type (Config, Test)

-- | Parse preprocessor arguments and write the test runner module.
run :: [String] -> IO ()
run processor_args = do
  name <- getProgName
  case processor_args of
    src : _ : dst : opts -> case parseConfig name opts of

      Left err -> do
        hPutStrLn stderr err
        exitFailure

      Right conf -> do
        stringed <- show <$> getListOfTests src conf
        tests    <- findTests src conf
        writeFile dst (tmpModule src conf tests stringed)

    _ -> do
      hPutStrLn stderr name
      exitFailure


-- | Generate the test runner module.
tmpModule :: FilePath -> Config -> [Test] -> String -> String
tmpModule src conf tests ts =
  (
    "{-# LINE 1 " . shows src . " #-}\n"
  . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  . showString "{-# LANGUAGE TemplateHaskell #-}\n"
  . showString "module Main where\n"
  . showString "import Test.Tasty.Discover\n"
  . importList tests conf
  . showString "main :: IO ()\n"
  . showString ("main = do $(defaultMainGeneratorFor \"tasty-discover\" " ++ ts ++ ")")
  ) "\n"

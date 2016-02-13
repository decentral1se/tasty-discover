{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Run (run) where

-- System
import System.IO
import System.Exit
import System.Environment  (getProgName)

-- Data
import           Data.String

-- Config
import Config              (Config, parseConfig, usage)

instance IsString ShowS where
  fromString = showString

run :: [String] -> IO ()
run processor_args = do
  name <- getProgName
  case processor_args of
    src : _ : dst : opts -> case parseConfig name opts of

      Left err -> do
        hPutStrLn stderr err
        exitFailure

      Right conf -> do
        writeFile dst (testModule src conf)

    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure


testModule :: FilePath -> Config -> String
testModule src _ =
  ( "{-# LINE 1 \"test/SomeTest.hs\" #-}\n"
  . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  . showString "{-# LANGUAGE TemplateHaskell #-}\n"
  . showString ("module Main where\n")
  . showString "import Test.Tasty\n"
  . showString "import Test.Tasty.HUnit\n"
  . showString "import Test.Tasty.QuickCheck\n"
  . showString "import Test.Tasty.TH\n"
  . showString "import SomeTest\n"
  . showString "main = $(defaultMainGenerator)"
  ) "\n"

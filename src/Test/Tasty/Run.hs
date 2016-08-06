{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Where we generate the test running boilerplate. Preprocessor arguments
-- arrive via the main function of `Test.Tasty.Discover`.
--
-- If you need to make `tasty-discover` do something new, it most likely needs
-- to happen here.

module Test.Tasty.Run (
    run
  , Test
  , defaultConfig
  , fileToTest
  , findTests
  , getFilesRecursive
  , getListOfTests
  , getTestFiles
  , importList
  , isValidModuleChar
  , isValidModuleName
  , parseConfig
  , stringifyTestList
  , testFile
  , testModule
  , tmpModule
  , Config(Config, configModuleSuffix)
  ) where

import           Test.Tasty.Config (Config(Config), configModuleSuffix, defaultConfig)
import           Test.Tasty.Parse  (parseConfig)
import           Test.Tasty.Prelude
import           Test.Tasty.Type
import           Test.Tasty.Util (importList, isValidModuleChar, isValidModuleName,
                                  getFilesRecursive, fileToTest, findTests,
                                  stringifyTestList, getListOfTests, getTestFiles)

-- | Accept some args and run the tests
--
-- >>> run ["w", "x", "y", "z"]
-- ...
run :: [String] -> IO ()
run processor_args = do
  name <- getProgName
  case processor_args of
    src : _ : dst : opts -> case parseConfig name opts of

      Left err -> do
        hPutStrLn stderr err
        exitFailure

      Right conf -> do
        stringed <- stringifyTestList $ getListOfTests src conf
        tests    <- findTests src conf
        writeFile dst (tmpModule src conf tests stringed)

    _ -> do
      hPutStrLn stderr name
      exitFailure


-- | The holy grail. This 'tmpModule' runs your tests
--
-- >>> tmpModule "test/Tasty.hs"
--               Config {configModuleName = Nothing}
--               [Test {testFile = "test/FooTest.hs", testModule = "Foo"}]
--               "[\"prop_one\"]"
-- ...
tmpModule :: FilePath -> Config -> [Test] -> String -> String
tmpModule src conf tests ts =
  (
    "{-# LINE 1 " . shows src . " #-}\n"
  . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  . showString "{-# LANGUAGE TemplateHaskell #-}\n"

  . showString "module Main where\n"
  . showString "import Test.Tasty.Discover\n"
  . importList tests (configModuleSuffix conf)

  . showString "main :: IO ()\n"
  . showString ("main = do $(defaultMainGeneratorFor \"tasty-discover\" " ++ ts ++ ")")

  ) "\n"

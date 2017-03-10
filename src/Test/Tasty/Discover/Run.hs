-- | Test discovery and runner boilerplate generator.

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Tasty.Discover.Run (
  run
, tmpModule
) where

import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Data.List (find, intercalate, isPrefixOf, nub)
import Data.Maybe (fromJust, mapMaybe)

import Test.Tasty.Discover.Parse (parseConfig)
import Test.Tasty.Discover.Util (importList, findTests, getListOfTests)
import Test.Tasty.Discover.Type (Config, Test)

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
        fns   <- getListOfTests src conf
        tests <- findTests src conf
        writeFile dst (tmpModule src conf tests fns)

    _ -> do
      hPutStrLn stderr name
      exitFailure


-- | Generate the test runner module.
tmpModule :: FilePath -> Config -> [Test] -> [String] -> String
tmpModule src conf tests fns =
  (
    showString "{-# LINE 1 " . shows src . showString " #-}\n"
  . showString "module Main where\n"
  . showString "import Test.Tasty\n"
  . showString (concat $ nub $ mapMaybe getTestImport fns)
  . importList tests conf
  . showString "main :: IO ()\n"
  . showString "main = do\n"
  . showString (concat $ mapMaybe buildSpecTree fns)
  . showString ("  defaultMain $ testGroup \"tasty-discover\" [\n      " ++ intercalate "\n    , " (map test fns) ++ "\n    ]")
  ) "\n"
  where testFunctions = [ ("prop_", ("testProperty", Just "Test.Tasty.QuickCheck"))
                        , ("case_", ("testCase", Just "Test.Tasty.HUnit"))
                        , ("test_", ("testGroup", Nothing))
                        , ("spec_", ("testSpec", Just "Test.Tasty.Hspec"))
                        ]
        findTestFunction fn = snd . fromJust $ find ((`isPrefixOf` fn) . fst) testFunctions
        getTestFunction = fst . findTestFunction
        getTestImport = fmap (\i -> "import " ++ i ++ "\n") . snd . findTestFunction
        buildSpecTree fn = if isSpec fn then Just $ "  tree_" ++ fn ++ " <- testSpec " ++ show (fixName fn) ++ " " ++ fn ++ "\n" else Nothing
        test fn = if isSpec fn then "tree_" ++ fn else getTestFunction fn ++ " " ++ show (fixName fn) ++ " " ++ fn
        fixName = replace '_' ' ' . tail . dropWhile (/= '_')
        replace b v = map (\i -> if b == i then v else i)
        isSpec = ("spec_" `isPrefixOf`)

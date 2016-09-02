-- | Utility functions.

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tasty.Util (
  importList
, findTests
, stringifyTestList
, getListOfTests
, getTestFiles
) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Char (isAlphaNum, isUpper)
import Data.List (intercalate, sort, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.String (IsString, fromString)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (splitDirectories, splitFileName, (</>))

import Test.Tasty.TH (extractTestFunctions)

import Test.Tasty.Config (Config, Config(configModuleSuffix))
import Test.Tasty.Type

-- | @TODO
instance IsString ShowS where
  fromString = showString

-- | Import statements for a list of tests.
--
-- >>> importList [Test {testFile = "test/SomeOtherTest.hs", testModule = "SomeOther"}]
-- "import qualified SomeOtherTest\n"
importList :: [Test] -> Maybe String -> ShowS
importList ts suffix = foldr (.) "" . map f $ ts
  where
    f :: Test -> ShowS
    f test = case suffix of
      Just suffix' -> "import " . showString (testModule test) . showString (suffix' ++ "\n")
      _            -> "import " . showString (testModule test) . "Test\n"


-- | Is 'c' a valid character in a Haskell module name?
--
-- >>> isValidModuleChar '-'
-- False
--
-- >>> isValidModuleChar 'A'
-- True
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Is 'cs' a valid Haskell module name?
--
-- >>> isValidModuleName "ModName"
-- True
--
-- >>> isValidModuleName "modName"
-- False
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

-- | All files under 'baseDir'.
--
-- >>> getFilesRecursive "test/"
-- ["FooTest.hs", "BarTest.hs"]
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)

-- | A test file becomes a Test type.
--
-- >>> fileToTest "test" Nothing "FooTest.hs"
-- Just (Test {testFile = "test/FooTest.hs", testModule = "Foo"})
--
-- >>> fileToTest "test" "MySuffix" "FooMySuffix.hs"
-- Just (Test {testFile = "test/FooMySuffix.hs", testModule = "Foo"})
fileToTest :: FilePath -> Maybe String -> FilePath -> Maybe Test
fileToTest dir suffix file = case reverse $ splitDirectories file of
  -- @TODO - REFACTOR
  x:xs -> case suffix of
            Just suffix' -> case
                stripSuffix (suffix' ++ ".hs")  x <|>
                stripSuffix (suffix' ++ ".lhs") x of
                    Just name | isValidModuleName name && all isValidModuleName xs ->
                        Just . Test (dir </> file) $
                            (intercalate "." . reverse) (name : xs)
                    _ -> Nothing
            _ -> case
                stripSuffix "Test.hs" x <|>
                stripSuffix "Test.lhs" x of
                    Just name | isValidModuleName name && all isValidModuleName xs ->
                        Just . Test (dir </> file) $
                            (intercalate "." . reverse) (name : xs)
                    _ -> Nothing
  _   -> Nothing
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suff str = reverse <$> stripPrefix (reverse suff) (reverse str)

-- | All tests that are not the 'src' file.
--
-- >>> findTests "test/Tasty.hs"
-- [Test {testFile = "test/FooTest.hs", testModule = "Foo"}]
findTests :: FilePath -> Config -> IO [Test]
findTests src conf =
  let (dir, file) = splitFileName src
      suffix      = configModuleSuffix conf
      tests       = mapMaybe $ fileToTest dir suffix
  in
    tests . filter (/= file) <$> getFilesRecursive dir

-- | A list of test function names as a String.
--
-- >>> stringifyTestList ["prop_one", "prop_two"]
-- "[\"prop_one\",\"prop_two\"]"
stringifyTestList :: IO [String] -> IO String
stringifyTestList = fmap show

-- | All test function names in 'src'.
--
-- >>> getListOfTests "test/Tasty.hs"
-- ["prop_one"]
getListOfTests :: FilePath -> Config -> IO [String]
getListOfTests src conf = do
    allFiles <- getTestFiles $ findTests src conf
    allTests <- mapM extractTestFunctions allFiles
    return $ concat allTests

-- | File paths for test files.
--
-- >>> getTestFiles $ findTests "test/Tasty.hs"
-- ["test/FooTest.hs"]
getTestFiles :: IO [Test] -> IO [FilePath]
getTestFiles = fmap (fmap testFile)

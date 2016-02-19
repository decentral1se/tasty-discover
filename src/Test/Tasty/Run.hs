{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tasty.Run (
    run

  -- for experimentation/testing
  , Test
  , stringifyTestList
  , getListOfTests
  , getTestFiles
  , findTests
  , tmpModule
  , fileToTest
  , getFilesRecursive
  , isValidModuleName
  , isValidModuleChar
  , importList
  , testFile
  , testModule
  , defaultConfig
  ) where

-- System
import System.IO
import System.Exit
import System.Environment         (getProgName)
import System.Directory           (
    doesFileExist
  , doesDirectoryExist
  , getDirectoryContents
  )
import System.FilePath            (
    splitDirectories
  , splitFileName
  , (</>)
  )

-- Data
import Data.String
import Data.Maybe
import Data.List                   (sort, intercalate, stripPrefix)
import Data.Char                   (isAlphaNum, isUpper)

-- Control
import Control.Applicative
import Control.Monad

-- Config
import Test.Tasty.Config           (Config, defaultConfig, parseConfig, usage)

-- Tasty
import Test.Tasty.TH

instance IsString ShowS where
  fromString = showString

data Test = Test {
  testFile   :: FilePath
, testModule :: String
} deriving (Eq, Show)

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
        stringed <- stringifyTestList $ getListOfTests src
        tests    <- findTests src
        print src
        print conf
        print tests
        print stringed
        writeFile dst (tmpModule src conf tests stringed)

    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure


-- | The holy grail. This 'tmpModule' runs your tests
--
-- >>> tmpModule "test/Tasty.hs"
--               Config {configModuleName = Nothing}
--               [Test {testFile = "test/FooTest.hs", testModule = "Foo"}]
--               "[\"prop_one\"]"
-- ...
tmpModule :: FilePath -> Config -> [Test] -> String -> String
tmpModule src conf tests stringed =
  ( "{-# LINE 1 " . shows src . " #-}\n"
  . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  . showString "{-# LANGUAGE TemplateHaskell #-}\n"
  . showString "module Main where\n"
  . showString "import Test.Tasty\n"
  . showString "import Test.Tasty.TH\n"
  . showString "import Test.Tasty.HUnit\n"
  . showString "import Test.Tasty.QuickCheck\n"
  . importList tests
  . showString "main :: IO ()\n"
  . showString ("main = $(defaultMainGeneratorFor) \"X\" =<< " ++ stringed ++ ")")
  ) "\n"

-- | A list of test function names as a String
--
-- >>> stringifyTestList ["prop_one", "prop_two"]
-- "[\"prop_one\",\"prop_two\"]"
stringifyTestList :: IO [String] -> IO String
stringifyTestList xs = fmap show xs

-- | All test function names in 'src'
--
-- >>> getListOfTests "test/Tasty.hs"
-- ["prop_one"]
getListOfTests :: FilePath -> IO [String]
getListOfTests src = do
    allFiles <- getTestFiles $ findTests src
    allTests <- mapM extractTestFunctions allFiles
    return $ concat allTests

-- | File paths for test files
--
-- >>> getTestFiles $ findTests "test/Tasty.hs"
-- ["test/FooTest.hs"]
getTestFiles :: IO [Test] -> IO [FilePath]
getTestFiles tests = fmap (fmap testFile) tests

-- | All tests that are not the 'src' file
--
-- >>> findTests "test/Tasty.hs"
-- [Test {testFile = "test/FooTest.hs", testModule = "Foo"}]
findTests :: FilePath -> IO [Test]
findTests src = do
  let (dir, file) = splitFileName src
  mapMaybe (fileToTest dir) . filter (/= file) <$> getFilesRecursive dir

-- | A test file becomes a Test type
--
-- >>> fileToTest "test" "FooTest.hs"
-- Just (Test {testFile = "test/FooTest.hs", testModule = "Foo"})
fileToTest :: FilePath -> FilePath -> Maybe Test
fileToTest dir file = case reverse $ splitDirectories file of
  x:xs -> case stripSuffix "Test.hs" x <|> stripSuffix "Test.lhs" x of
    Just name | isValidModuleName name && all isValidModuleName xs ->
      Just . Test (dir </> file) $ (intercalate "." . reverse) (name : xs)
    _ -> Nothing
  _ -> Nothing
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

-- | All files under 'baseDir'
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

-- | Is 'cs' a valid Haskell module name?
-- | Reference - `Cabal.Distribution.ModuleName` (http://git.io/bj34)
--
-- >>> isValidModuleName "ModName"
-- True
--
-- >>> isValidModuleName "modName"
-- False
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

-- | Is 'c' a valid character in a Haskell module name?
--
-- >>> isValidModuleChar '-'
-- False
--
-- >>> isValidModuleChar 'A'
-- True
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Import statements for a list of tests
--
-- >>> importList [Test {testFile = "test/SomeOtherTest.hs", testModule = "SomeOther"}]
-- "import qualified SomeOtherTest\n"
importList :: [Test] -> ShowS
importList = foldr (.) "" . map f
  where
    f :: Test -> ShowS
    f test = "import qualified " . showString (testModule test) . "Test\n"

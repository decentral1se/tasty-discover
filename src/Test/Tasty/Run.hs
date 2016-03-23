{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Where we generate the test running boilerplate. Preprocessor arguments
-- arrive via the main function of `Test.Tasty.Discover`.
--
-- If you need to make `tasty-discover` do something new, it most likely needs
-- to happen here.

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
  , parseConfig
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
import Test.Tasty.Config           (
    Config
  , configModuleSuffix
  , defaultConfig
  , parseConfig
  , usage
  )

-- Tasty
import Test.Tasty.TH               (extractTestFunctions)

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
        stringed <- stringifyTestList $ getListOfTests src conf
        tests    <- findTests src conf
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
tmpModule src conf tests ts =
  (
    "{-# LINE 1 " . shows src . " #-}\n"
  . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  . showString "{-# LANGUAGE TemplateHaskell #-}\n"

  . showString "module Main where\n"
  . showString "import Test.Tasty.Discover\n"
  . importList tests (configModuleSuffix conf)

  . showString "main :: IO ()\n"
  . showString "main = do\n"
  . showString ("\t$(defaultMainGeneratorFor \"tasty-discover\" " ++ ts ++ ")")

  ) "\n"

-- | A list of test function names as a String
--
-- >>> stringifyTestList ["prop_one", "prop_two"]
-- "[\"prop_one\",\"prop_two\"]"
stringifyTestList :: IO [String] -> IO String
stringifyTestList = fmap show

-- | All test function names in 'src'
--
-- >>> getListOfTests "test/Tasty.hs"
-- ["prop_one"]
getListOfTests :: FilePath -> Config -> IO [String]
getListOfTests src conf = do
    allFiles <- getTestFiles $ findTests src conf
    allTests <- mapM extractTestFunctions allFiles
    return $ concat allTests

-- | File paths for test files
--
-- >>> getTestFiles $ findTests "test/Tasty.hs"
-- ["test/FooTest.hs"]
getTestFiles :: IO [Test] -> IO [FilePath]
getTestFiles = fmap (fmap testFile)

-- | All tests that are not the 'src' file
--
-- >>> findTests "test/Tasty.hs"
-- [Test {testFile = "test/FooTest.hs", testModule = "Foo"}]
findTests :: FilePath -> Config -> IO [Test]
findTests src conf = do
  let (dir, file) = splitFileName src
  mapMaybe (fileToTest dir (configModuleSuffix conf)) . filter (/= file) <$> getFilesRecursive dir

-- | A test file becomes a Test type
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
importList :: [Test] -> Maybe String -> ShowS
importList ts suffix = foldr (.) "" . map f $ ts
  where
    f :: Test -> ShowS
    f test = case suffix of
      Just suffix' -> "import " . showString (testModule test) . showString (suffix' ++ "\n")
      _            -> "import " . showString (testModule test) . "Test\n"

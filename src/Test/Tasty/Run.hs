{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tasty.Run (
    run

  -- for experimentation/testing
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
import Test.Tasty.Config  (Config, defaultConfig, parseConfig, usage)

instance IsString ShowS where
  fromString = showString

data Test = Test {
  testFile   :: FilePath
, testModule :: String
} deriving (Eq, Show)

run :: [String] -> IO ()
run processor_args = do
  name <- getProgName
  case processor_args of
    src : _ : dst : opts -> case parseConfig name opts of

      Left err -> do
        hPutStrLn stderr err
        exitFailure

      Right conf -> do
        tests <- findTests src
        writeFile dst (tmpModule src conf tests)

    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure


tmpModule :: FilePath -> Config -> [Test] -> String
tmpModule src conf tests =
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
  . showString "main = $(defaultMainGenerator)"
  ) "\n"

findTests :: FilePath -> IO [Test]
findTests src = do
  let (dir, file) = splitFileName src
  mapMaybe (fileToTest dir) . filter (/= file) <$> getFilesRecursive dir

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

-- | @TODO
-- getFilesRecursive "./test"
-- ["SomeTest.hs", "Tasty.hs"]
-- @TODO - this should be Maybe [FilePath]
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)

-- | @TODO
-- | See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
--
-- isValidModuleName "ModName"
-- True
--
-- isValidModuleName "modName"
-- False
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

-- | @TODO
-- isValidModuleChar '-'
-- False
--
-- isValidModuleChar 'A'
-- True
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Generate imports for a list of specs.
importList :: [Test] -> ShowS
importList = foldr (.) "" . map f
  where
    f :: Test -> ShowS
    f test = "import qualified " . showString (testModule test) . "Test\n"

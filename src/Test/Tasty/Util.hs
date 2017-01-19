-- | Utility functions.

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tasty.Util (
  importList
, findTests
, getListOfTests

-- Testing purposes
, fileToTest
, getFilesRecursive
, isValidModuleChar
, isValidModuleName
) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Char (isAlphaNum, isUpper)
import Data.List (intercalate, sort, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.String (IsString, fromString)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (splitDirectories, splitFileName, (</>))
import System.FilePath.Posix (splitExtension)

import Test.Tasty.TH (extractTestFunctions)

import Test.Tasty.Config (Config(..))
import Test.Tasty.Type

instance IsString ShowS where
  fromString = showString

-- | Import statements for a list of tests.
importList :: [Test] -> Config -> ShowS
importList ts config =
    foldr ((.) . f) "" ts
  where
    f :: Test -> ShowS
    f test = if noModuleSuffix config then
      "import " . showString (testModule test) . "\n"
    else
      case configModuleSuffix config of
        Just suffix' -> "import " . showString (testModule test) . showString (suffix' ++ "\n")
        _            -> "import " . showString (testModule test) . "Test\n"


-- | Is 'c' a valid character in a Haskell module name?
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Is 'cs' a valid Haskell module name?
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

-- | All files under 'baseDir'.
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)

-- | Convert a file to a File type.
fileToTest :: FilePath -> Config -> FilePath -> Maybe Test
fileToTest dir conf file =
    let
        suffix :: Maybe String
        suffix   = configModuleSuffix conf

        noModule :: Bool
        noModule = noModuleSuffix conf

        files :: [FilePath]
        files    = reverse $ splitDirectories file
    in
      if noModule then catchAll files else case suffix of
          Just suffix' -> filterBySuffix suffix' files
          Nothing      -> filterBySuffix "Test" files
    where
      filterBySuffix :: String -> [FilePath] -> Maybe Test
      filterBySuffix suffix files =
        case files of
          x:xs ->  case
            stripSuffix (suffix ++ ".hs") x <|> stripSuffix (suffix ++ ".lhs") x of
              Just name | isValidModuleName name && all isValidModuleName xs ->
                let pathComponents = reverse (name : xs)
                    moduleName = intercalate "." pathComponents
                in if isIgnoredModule pathComponents
                     then Nothing
                     else Just . Test (dir </> file) $ moduleName
              _ -> Nothing
          _    -> Nothing

      isIgnoredModule :: [FilePath] -> Bool
      isIgnoredModule pathComponents =
        let moduleName = intercalate "." pathComponents
        in moduleName `elem` ignoredModules conf

      stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
      stripSuffix suff str = reverse <$> stripPrefix (reverse suff) (reverse str)

      catchAll :: [FilePath] -> Maybe Test
      catchAll (x:xs) =
        let name = fst $ splitExtension x
            pathComponents = reverse (name : xs)
        in
          if isValidModuleName name
             && all isValidModuleName xs
             && not (isIgnoredModule pathComponents) then
            Just . Test (dir </> file) $ (intercalate "." . reverse) (name : xs)
          else Nothing
      catchAll _ = Nothing

-- | All test modules under 'dir'.
findTests :: FilePath -> Config -> IO [Test]
findTests path config =
  let (dir, file) = splitFileName path
      tests       = mapMaybe $ fileToTest dir config
  in
    tests . filter (/= file) <$> getFilesRecursive dir

-- | All test function names in 'src'.
getListOfTests :: FilePath -> Config -> IO [String]
getListOfTests src conf = do
    allFiles <- fmap testFile <$> findTests src conf
    allTests <- mapM extractTestFunctions allFiles
    return $ concat allTests

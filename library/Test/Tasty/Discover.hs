-- | Automatic test discovery and runner for the tasty framework.
module Test.Tasty.Discover where

import Data.List (isPrefixOf, isSuffixOf, nub, intercalate, dropWhileEnd)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Data.Traversable (for)
import System.FilePath ((</>), takeDirectory)
import Test.Tasty.Generator (generators, showSetup, getGenerators)
import Test.Tasty.Type (Config(..), Generator(..), Test(..), mkTest)

generateTestDriver :: String -> [String] -> FilePath -> [Test] -> String
generateTestDriver modname is src tests =
  let generators' = getGenerators tests
      testNumVars = map (("t"++) . show) [(0 :: Int)..]
  in
    concat
      [ "{-# LINE 1 \"" ++ src ++ "\" #-}\n"
      , "{-# LANGUAGE FlexibleInstances #-}\n"
      , "module " ++ modname ++ " (main, ingredients, tests) where\n"
      , "import Prelude\n"
      , "import qualified Test.Tasty as T\n"
      , "import qualified Test.Tasty.Ingredients as T\n"
      , unlines $ map generatorImport generators'
      , showImports (map ingredientImport is ++ map testModule tests)
      , unlines $ map generatorClass generators'
      , "tests :: IO T.TestTree\n"
      , "tests = do\n"
      , unlines $ zipWith showSetup tests testNumVars
      , "  pure $ T.testGroup \"" ++ src ++ "\" ["
      , intercalate "," $ zipWith (curry snd) tests testNumVars
      , "]\n"
      , concat
        [ "ingredients :: [T.Ingredient]\n"
        , "ingredients = " ++ ingredients is ++ "\n"
        , "main :: IO ()\n"
        , "main = tests >>= T.defaultMainWithIngredients ingredients\n"
        ]
      ]

filterHidden :: String -> Bool
filterHidden file = head file /= '.'

filterIgnored :: String -> [FilePath] -> Bool
filterIgnored file ignore' = file `elem` ignore'

filesBySuffix :: FilePath -> [FilePath] -> [String] -> IO [FilePath]
filesBySuffix dir ignore suffixes = do
  let unWanted file = filterHidden file || filterIgnored file ignore
  entries <- filter unWanted <$> getDirectoryContents dir
  found <- for entries $ \entry -> do
    let dir' = dir </> entry
    dirExists <- doesDirectoryExist dir'
    if dirExists then
      map (entry </>) <$> filesBySuffix dir' ignore suffixes
    else
      pure []
  pure $ filter (\x -> any (`isSuffixOf` x) suffixes) entries ++ concat found

allFiles :: FilePath -> IO [FilePath]
allFiles dir = do
  entries <- filter filterHidden <$> getDirectoryContents dir
  found <- for entries $ \entry -> do
    let dir' = dir </> entry
    dirExists <- doesDirectoryExist dir'
    if dirExists then
      map (entry </>) <$> allFiles dir'
    else
      pure []
  pure $ entries ++ concat found

findTests :: FilePath -> Config -> IO [Test]
findTests src config = do
  let dir = takeDirectory src
      suffixes = testFileSuffixes (moduleSuffix config)
      ignore = ignoredModules config
  files <-
    if noModuleSuffix config then
      allFiles dir
    else
      filesBySuffix dir ignore suffixes
  concat <$> traverse (extract dir) files
  where
    extract dir file = extractTests file <$> readFile (dir </> file)

extractTests :: FilePath -> String -> [Test]
extractTests file = mkTestDeDuped . isKnownPrefix . parseTest
  where
    mkTestDeDuped = map (mkTest file) . nub
    isKnownPrefix = filter (\g -> any (checkPrefix g) generators)
    checkPrefix g = (`isPrefixOf` g) . generatorPrefix
    parseTest     = map fst . concatMap lex . lines

testFileSuffixes :: Maybe String -> [String]
testFileSuffixes suffix = (++) <$> suffixes <*> [".lhs", ".hs"]
  where
    defaults = ["Spec", "Test"]
    suffixes = case suffix of
      Just s  -> defaults ++ [s]
      Nothing -> defaults

showImports :: [String] -> String
showImports mods = unlines $ nub $ map (\m -> "import qualified " ++ m ++ "\n") mods

ingredientImport :: String -> String
ingredientImport = init . dropWhileEnd (/= '.')

ingredients :: [String] -> String
ingredients is = concat $ map (++":") is ++ ["T.defaultIngredients"]

-- | Automatic test discovery and runner for the tasty framework.
module Test.Tasty.Discover where

import           Data.List            (dropWhileEnd, intercalate, isPrefixOf,
                                       isSuffixOf, nub)
import           Data.Traversable     (for)
import           System.Directory     (doesDirectoryExist, getDirectoryContents)
import           System.FilePath      (takeDirectory, (</>))
import           Test.Tasty.Config    (Config (..))
import           Test.Tasty.Generator (Generator (..), Test (..), generators,
                                       getGenerators, mkTest, showSetup)

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
      , intercalate "," $ showTests tests testNumVars
      , "]\n"
      , concat
        [ "ingredients :: [T.Ingredient]\n"
        , "ingredients = " ++ ingredients is ++ "\n"
        , "main :: IO ()\n"
        , "main = tests >>= T.defaultMainWithIngredients ingredients\n"
        ]
      ]

addSuffixes :: [String] -> [String]
addSuffixes modules = (++) <$> modules <*> [".lhs", ".hs"]

isHidden :: FilePath -> Bool
isHidden filename = head filename /= '.'

filesBySuffix :: FilePath -> [String] -> IO [FilePath]
filesBySuffix dir suffixes = do
  entries <- filter isHidden <$> getDirectoryContents dir
  found <- for entries $ \entry -> do
    let dir' = dir </> entry
    dirExists <- doesDirectoryExist dir'
    if dirExists then
      map (entry </>) <$> filesBySuffix dir' suffixes
    else
      pure []
  pure $ filter (\x -> any (`isSuffixOf` x) suffixes) entries ++ concat found

isIgnored :: [FilePath] -> String -> Bool
isIgnored ignores filename = filename `notElem` addSuffixes ignores

findTests :: FilePath -> Config -> IO [Test]
findTests src config = do
  let dir      = takeDirectory src
      suffixes = testFileSuffixes (moduleSuffix config)
      ignores  = ignoredModules config
  files <-
    if noModuleSuffix config
    then filter isHidden <$> getDirectoryContents dir
    else filesBySuffix dir suffixes
  let files' = filter (isIgnored ignores) files
  concat <$> traverse (extract dir) files'
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
testFileSuffixes suffix = addSuffixes suffixes
  where
    suffixes = case suffix of
      Just suffix' -> [suffix']
      Nothing      -> ["Spec", "Test"]

showImports :: [String] -> String
showImports mods = unlines $ nub $ map (\m -> "import qualified " ++ m ++ "\n") mods

ingredientImport :: String -> String
ingredientImport = init . dropWhileEnd (/= '.')

ingredients :: [String] -> String
ingredients is = concat $ map (++":") is ++ ["T.defaultIngredients"]

showTests :: [Test] -> [String] -> [String]
showTests tests testNumVars = zipWith (curry snd) tests testNumVars

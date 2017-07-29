-- | Automatic test discovery and runner for the tasty framework.

module Test.Tasty.Discover (
  -- * Main Test Generator
  generateTestDriver

  -- * For Testing Purposes Only
  , ModuleTree (..)
  , findTests
  , addSuffixes
  , mkModuleTree
  , showTests
  ) where

import           Data.List            (dropWhileEnd, intercalate, isPrefixOf,
                                       isSuffixOf, nub)
import qualified Data.Map.Strict      as M
import           Data.Traversable     (for)
import           System.Directory     (doesDirectoryExist, getDirectoryContents)
import           System.FilePath      (takeDirectory, (</>))
import           Test.Tasty.Config    (Config (..))
import           Test.Tasty.Generator (Generator (..), Test (..), generators,
                                       getGenerators, mkTest, showSetup)

-- | Main function generator, along with all the boilerplate which
-- which will run the discovered tests.
generateTestDriver :: Config -> String -> [String] -> FilePath -> [Test] -> String
generateTestDriver config modname is src tests =
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
      , intercalate "," $ showTests config tests testNumVars
      , "]\n"
      , concat
        [ "ingredients :: [T.Ingredient]\n"
        , "ingredients = " ++ ingredients is ++ "\n"
        , "main :: IO ()\n"
        , "main = tests >>= T.defaultMainWithIngredients ingredients\n"
        ]
      ]

-- | Append specified suffixes to a list of test modules.
addSuffixes :: [String] -> [String]
addSuffixes modules = (++) <$> modules <*> [".lhs", ".hs"]

-- | Is the file in question a hidden file?
isHidden :: FilePath -> Bool
isHidden filename = head filename /= '.'

-- | Filter modules by suffix
filesBySuffix :: FilePath -> [String] -> IO [FilePath]
filesBySuffix dir suffixes = do
  entries <- filter isHidden <$> getDirectoryContents dir
  fmap concat $ for entries $ \entry -> do
    let dir' = dir </> entry
    dirExists <- doesDirectoryExist dir'
    if dirExists then
      map (entry </>) <$> filesBySuffix dir' suffixes
    else if any (`isSuffixOf` entry) suffixes then
      pure [entry]
    else
      pure []

-- | Is a particular module being ignored?
isIgnored :: [FilePath] -> String -> Bool
isIgnored ignores filename = filename `notElem` addSuffixes ignores

-- | Discover tests.
findTests :: FilePath -> Config -> IO [Test]
findTests src config = do
  let dir      = takeDirectory src
      suffixes = testFileSuffixes config
      ignores  = ignoredModules config
  files <- filter (isIgnored ignores) <$> filesBySuffix dir suffixes
  concat <$> traverse (extract dir) files
  where
    extract dir file = extractTests file <$> readFile (dir </> file)

-- | Extract the test names from discovered modules.
extractTests :: FilePath -> String -> [Test]
extractTests file = mkTestDeDuped . isKnownPrefix . parseTest
  where
    mkTestDeDuped = map (mkTest file) . nub
    isKnownPrefix = filter (\g -> any (checkPrefix g) generators)
    checkPrefix g = (`isPrefixOf` g) . generatorPrefix
    parseTest     = map fst . concatMap lex . lines

-- | Consider the suffix configuration and deal with test modules.
testFileSuffixes :: Config -> [String]
testFileSuffixes config = if noModuleSuffix config
    then [""]
    else addSuffixes suffixes
  where
    suffixes = case moduleSuffix config of
      Just suffix' -> [suffix']
      Nothing      -> ["Spec", "Test"]

-- | Show the imports.
showImports :: [String] -> String
showImports mods = unlines $ nub $ map (\m -> "import qualified " ++ m ++ "\n") mods

-- | Retrieve the ingredient name.
ingredientImport :: String -> String
ingredientImport = init . dropWhileEnd (/= '.')

-- | Ingredients to be included.
ingredients :: [String] -> String
ingredients is = concat $ map (++":") is ++ ["T.defaultIngredients"]

-- | Show the tests.
showTests :: Config -> [Test] -> [String] -> [String]
showTests config tests testNumVars = if treeDisplay config
  then showModuleTree $ mkModuleTree tests testNumVars
  else zipWith (curry snd) tests testNumVars

newtype ModuleTree = ModuleTree (M.Map String (ModuleTree, [String]))
  deriving (Eq, Show)

showModuleTree :: ModuleTree -> [String]
showModuleTree (ModuleTree mdls) = map showModule $ M.assocs mdls
  where
    -- special case, collapse to mdl.submdl
    showModule (mdl, (ModuleTree subMdls, [])) | M.size subMdls == 1 =
      let [(subMdl, (subSubTree, testVars))] = M.assocs subMdls
      in showModule (mdl ++ '.' : subMdl, (subSubTree, testVars))
    showModule (mdl, (subTree, testVars)) = concat
      [ "T.testGroup \"", mdl
      , "\" [", intercalate "," (showModuleTree subTree ++ testVars), "]" ]

mkModuleTree :: [Test] -> [String] -> ModuleTree
mkModuleTree tests testVars = ModuleTree $
    foldr go M.empty $ zipWith (\t tVar -> (testModule t, tVar)) tests testVars
  where
    go (mdl, tVar) mdls = M.insertWith merge key val mdls
      where
        (key, val) = case break (== '.') mdl of
          (_, []) -> (mdl, (ModuleTree M.empty, [tVar]))
          (topMdl, '.':subMdl) -> (topMdl, (ModuleTree $ go (subMdl, tVar) M.empty, []))
          _ -> error "impossible case in mkModuleTree.go.key"
    merge (ModuleTree mdls1, tVars1) (ModuleTree mdls2, tVars2) =
      (ModuleTree $ M.unionWith merge mdls1 mdls2, tVars1 ++ tVars2)

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigTest where

import           Data.List             (isInfixOf)
import qualified Data.Map.Strict       as M
import           Test.Tasty.Config
import           Test.Tasty.Discover   (ModuleTree (..), findTests,
                                        generateTestDriver, mkModuleTree,
                                        showTests)
import           Test.Tasty.Generator  (Test (..), mkTest)
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

spec_customModuleSuffix :: Spec
spec_customModuleSuffix =
  describe "Module suffix configuration" $
  it "Filters discovered tests by specified suffix" $ do
    let customSuffixConfig = defaultConfig { moduleSuffix = Just "DoesntExist"}
    discoveredTests <- findTests "test/SubMod/" customSuffixConfig
    discoveredTests `shouldBe` []

spec_customModuleName :: Spec
spec_customModuleName =
  describe "Module name configuration" $
  it "Creates a generated main function with the specified name" $ do
    let generatedModule = generateTestDriver defaultConfig "FunkyModuleName" [] "test/" []
    "FunkyModuleName" `shouldSatisfy` (`isInfixOf` generatedModule)

spec_ignoreModule :: Spec
spec_ignoreModule =
  describe "Module ignore configuration" $
  it "Ignores tests in modules with the specified suffix" $ do
    let ignoreModuleConfig = defaultConfig { ignoredModules = ["PropTest"] }
    discoveredTests <- findTests "test/SubMod/" ignoreModuleConfig
    discoveredTests `shouldBe` []

spec_findsTests :: Spec
spec_findsTests =
  describe "Test discovery" $
  it "Discovers tests" $ do
    let expectedTest = mkTest "PropTest" "prop_additionAssociative"
    discoveredTests <- findTests "test/SubMod/" defaultConfig
    discoveredTests `shouldBe` [expectedTest]

spec_noModuleSuffix :: Spec
spec_noModuleSuffix =
  describe "No module suffix configuration" $
  it "Discovers tests in modules with every suffix" $ do
    let noModuleSuffixConfig = defaultConfig { noModuleSuffix = True }
        expectedTests = [ mkTest "FooBaz" "prop_additionCommutative"
                        , mkTest "FooBaz" "prop_multiplationDistributiveOverAddition"
                        , mkTest "PropTest" "prop_additionAssociative"
                        ]
    moreDiscoveredTests <- findTests "test/SubMod/" noModuleSuffixConfig
    moreDiscoveredTests `shouldSatisfy` all (`elem` expectedTests)

unit_noTreeDisplayDefault :: IO ()
unit_noTreeDisplayDefault = do
  let config = defaultConfig { noModuleSuffix = True }
  tests <- findTests "test/SubMod/" config
  let testNumVars = map (('t' :) . show) [(0::Int)..]
      trees = showTests config tests testNumVars
  length trees @?= 3

unit_treeDisplay :: IO ()
unit_treeDisplay = do
  let config = defaultConfig { noModuleSuffix = True, treeDisplay = True }
  tests <- findTests "test/SubMod/" config
  let testNumVars = map (('t' :) . show) [(0::Int)..]
      trees = showTests config tests testNumVars
  length trees @?= 2

prop_mkModuleTree :: ModuleTree -> Property
prop_mkModuleTree mtree =
    let (tests, testVars) = unzip $ flattenTree mtree
    in mkModuleTree tests testVars === mtree
  where
    flattenTree (ModuleTree mp) = M.assocs mp >>= flattenModule
    flattenModule (mdl, (subTree, testVars)) = concat
      [ map (\(Test subMdl _, tVar) -> (Test (mdl ++ '.':subMdl) "-", tVar)) (flattenTree subTree)
      , map (\tVar -> (Test mdl "-", tVar)) testVars ]

instance Arbitrary ModuleTree where
  arbitrary = sized $ \size ->
      resize (min size 12) (ModuleTree . M.fromList <$> listOf1 mdlGen)
    where
      mdlGen = sized $ \size -> do
        mdl <- listOf1 (elements ['a'..'z'])
        subTree <- if size == 0
          then pure $ ModuleTree M.empty
          else resize (size `div` 2) arbitrary
        tVars <- listOf1 (listOf1 arbitrary)
        pure (mdl, (subTree, tVars))

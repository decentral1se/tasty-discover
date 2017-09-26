{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigTest where

import           Data.List             (isInfixOf, sort)
import qualified Data.Map.Strict       as M
import           Test.Tasty.Config
import           Test.Tasty.Discover   (ModuleTree (..), findTests,
                                        generateTestDriver, mkModuleTree,
                                        showTests)
import           Test.Tasty.Generator  (Test (..), mkTest)
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

spec_modules :: Spec
spec_modules =
  describe "Test discovery" $
  it "Discovers tests" $ do
    let expectedTests = [ mkTest "PropTest.hs" "prop_additionAssociative",
                          mkTest "SubSubMod/PropTest.hs" "prop_additionCommutative" ]
        config        = defaultConfig { modules = Just "*Test.hs" }
    discoveredTests <- findTests "test/SubMod/" config
    sort discoveredTests `shouldBe` sort expectedTests

spec_ignores :: Spec
spec_ignores =
  describe "Module ignore configuration" $
  it "Ignores tests in modules with the specified suffix" $ do
    let ignoreModuleConfig = defaultConfig { ignores = Just "*.hs" }
    discoveredTests <- findTests "test/SubMod/" ignoreModuleConfig
    discoveredTests `shouldBe` []

spec_badModuleGlob :: Spec
spec_badModuleGlob =
  describe "Module suffix configuration" $
  it "Filters discovered tests by specified suffix" $ do
    let badGlobConfig = defaultConfig { modules = Just "DoesntExist*.hs" }
    discoveredTests <- findTests "test/SubMod/" badGlobConfig
    discoveredTests `shouldBe` []

spec_customModuleName :: Spec
spec_customModuleName =
  describe "Module name configuration" $
  it "Creates a generated main function with the specified name" $ do
    let generatedModule = generateTestDriver defaultConfig "FunkyModuleName" [] "test/" []
    "FunkyModuleName" `shouldSatisfy` (`isInfixOf` generatedModule)

unit_noTreeDisplayDefault :: IO ()
unit_noTreeDisplayDefault = do
  tests <- findTests "test/SubMod/" defaultConfig
  let testNumVars = map (('t' :) . show) [(0::Int)..]
      trees = showTests defaultConfig tests testNumVars
  length trees @?= 4

unit_treeDisplay :: IO ()
unit_treeDisplay = do
  let config = defaultConfig { treeDisplay = True }
  tests <- findTests "test/SubMod/" config
  let testNumVars = map (('t' :) . show) [(0::Int)..]
      trees = showTests config tests testNumVars
  length trees @?= 3

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

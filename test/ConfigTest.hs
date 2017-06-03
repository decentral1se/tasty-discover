{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConfigTest where

import           Data.List             (isInfixOf)
import qualified Data.Map.Strict       as M
import           Test.Tasty.Config
import           Test.Tasty.Discover   (ModuleTree (..), findTests,
                                        generateTestDriver, mkModuleTree,
                                        showTests)
import           Test.Tasty.Generator  (Test (..), mkTest)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

unit_noModuleSuffixEmptyList :: IO ()
unit_noModuleSuffixEmptyList = do
  actual <- findTests "test/SubMod/" (defaultConfig { moduleSuffix = Just "DoesntExist"})
  actual @?= []

unit_differentGeneratedModule :: Assertion
unit_differentGeneratedModule = assertBool "" ("FunkyModuleName" `isInfixOf` generatedModule)
  where generatedModule = generateTestDriver defaultConfig "FunkyModuleName" [] "test/" []

unit_ignoreAModule :: IO ()
unit_ignoreAModule = do
  actual <- findTests "test/SubMod/" (defaultConfig { ignoredModules = ["PropTest"] })
  actual @?= []

unit_noModuleSuffix :: IO ()
unit_noModuleSuffix  = do
  actual1 <- findTests "test/SubMod/" defaultConfig
  actual1 @?= [mkTest "PropTest" "prop_additionAssociative"]

  actual2 <- findTests "test/SubMod/" (defaultConfig { noModuleSuffix = True })
  let expected = [ mkTest "FooBaz" "prop_additionCommutative"
                 , mkTest "FooBaz" "prop_multiplationDistributiveOverAddition"
                 , mkTest "PropTest" "prop_additionAssociative" ]
  assertBool "" $ all (`elem` expected) actual2

unit_noModuleSuffixRecurseDirs :: IO ()
unit_noModuleSuffixRecurseDirs = do
  tests <- findTests "test/" (defaultConfig { noModuleSuffix = True })
  assertBool "" $ elem (mkTest "SubMod/FooBaz" "prop_additionCommutative") tests

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

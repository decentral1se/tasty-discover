module ConfigTest where

import           Data.List            (isInfixOf)
import           Test.Tasty.Config
import           Test.Tasty.Discover  (findTests, generateTestDriver)
import           Test.Tasty.Generator (mkTest)
import           Test.Tasty.HUnit

unit_noModuleSuffixEmptyList :: IO ()
unit_noModuleSuffixEmptyList = do
  actual <- findTests "test/SubMod/" (defaultConfig { moduleSuffix = Just "DoesntExist"})
  actual @?= []

unit_differentGeneratedModule :: Assertion
unit_differentGeneratedModule = assertBool "" ("FunkyModuleName" `isInfixOf` generatedModule)
  where generatedModule = generateTestDriver "FunkyModuleName" [] "test/" []

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
                 , mkTest "PropTest" "prop_additionAssociative" ]
  assertBool "" $ all (`elem` expected) actual2

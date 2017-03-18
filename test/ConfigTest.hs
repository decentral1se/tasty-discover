module ConfigTest where

import Data.List (isInfixOf)
import Test.Tasty.Discover (findTests, generateTestDriver)
import Test.Tasty.HUnit
import Test.Tasty.Config
import Test.Tasty.Generator (mkTest)

case_noModuleSuffixEmptyList :: IO ()
case_noModuleSuffixEmptyList = do
  actual <- findTests "test/SubMod/" (defaultConfig { moduleSuffix = Just "DoesntExist"})
  actual @?= []

case_differentGeneratedModule :: Assertion
case_differentGeneratedModule = assertBool "" ("FunkyModuleName" `isInfixOf` generatedModule)
  where generatedModule = generateTestDriver "FunkyModuleName" [] "test/" []

case_ignoreAModule :: IO ()
case_ignoreAModule = do
  actual <- findTests "test/SubMod/" (defaultConfig { ignoredModules = ["PropTest"] })
  actual @?= []

case_noModuleSuffix :: IO ()
case_noModuleSuffix  = do
  actual1 <- findTests "test/SubMod/" defaultConfig
  actual1 @?= [mkTest "PropTest" "prop_addition_is_associative"]

  actual2 <- findTests "test/SubMod/" config2
  actual2 @?= [ mkTest "FooBaz" "prop_addition_is_commutative"
             , mkTest "PropTest" "prop_addition_is_associative" ]
  where
    config1 = Config Nothing Nothing [] [] False False
    config2 = Config Nothing Nothing [] [] True False

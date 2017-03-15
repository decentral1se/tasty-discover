module ConfigTest where

import Data.List (isInfixOf)
import Test.Tasty.Discover (findTests, generateTestDriver)
import Test.Tasty.HUnit
import Test.Tasty.Type

case_noModuleSuffixEmptyList :: IO ()
case_noModuleSuffixEmptyList = do
  actual <- findTests "test/SubMod/" config
  actual @?= []
  where
    config = Config (Just "DoesntExist") Nothing [] [] False False

case_differentGeneratedModule :: Assertion
case_differentGeneratedModule = assertBool "Specified module is used" test
  where test = "FunkyModuleName" `isInfixOf` generatedModule
        generatedModule = generateTestDriver "FunkyModuleName" [] "test/" []

case_ignoreAModule :: IO ()
case_ignoreAModule = do
  actual <- findTests "test/SubMod/" config
  actual @?= []
  where
    config = Config Nothing Nothing ["PropTest"] [] False False

case_noModuleSuffix :: IO ()
case_noModuleSuffix  = do
  actual1 <- findTests "test/SubMod/" config1
  actual1 @?= [mkTest "PropTest" "prop_addition_is_associative"]

  actual2 <- findTests "test/SubMod/" config2
  actual2 @?= [ mkTest "FooBaz" "prop_addition_is_commutative"
             , mkTest "PropTest" "prop_addition_is_associative" ]
  where
    config1 = Config Nothing Nothing [] [] False False
    config2 = Config Nothing Nothing [] [] True False

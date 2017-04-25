module ConfigTest where

import           Data.List            (isInfixOf)
import           Test.Tasty.Config
import           Test.Tasty.Discover  (findTests, generateTestDriver)
import           Test.Tasty.Generator (mkTest)
import           Test.Tasty.HUnit

unit_differentGeneratedModule :: Assertion
unit_differentGeneratedModule =
  assertBool "" ("FunkyModuleName" `isInfixOf` generatedModule)
  where
    generatedModule = generateTestDriver "FunkyModuleName" [] "test/" []

unit_ignoreAModule :: IO ()
unit_ignoreAModule = do
  let config = defaultConfig { ignoredModules = ["PropTest"] }
  actual <- findTests "test/SubMod/" config
  actual @?= []

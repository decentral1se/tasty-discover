module Test.Tasty.Generator
  ( Generator(..)
  , generators
  , showSetup
  , getGenerator
  , getGenerators
  , Test(..)
  , mkTest,
  ) where

import           Data.Function   (on)
import           Data.List       (find, groupBy, isPrefixOf, sortOn)
import           Data.Maybe      (fromJust)
import           System.FilePath (dropExtension, pathSeparator)

data Test = Test
  { testModule   :: String
  , testFunction :: String
  } deriving (Eq, Show)

mkTest :: FilePath -> String -> Test
mkTest = Test . chooser pathSeparator '.' . dropExtension
  where chooser c1 c2 = map $ \c3 -> if c3 == c1 then c2 else c3

data Generator = Generator
  { generatorPrefix :: String
  , generatorImport :: String
  , generatorClass  :: String
  , generatorSetup  :: Test -> String
  }

qualifyFunction :: Test -> String
qualifyFunction t = testModule t ++ "." ++ testFunction t

name :: Test -> String
name = chooser '_' ' ' . tail . dropWhile (/= '_') . testFunction
  where chooser c1 c2 = map $ \c3 -> if c3 == c1 then c2 else c3

getGenerator :: Test -> Generator
getGenerator t = fromJust $ getPrefix generators
  where getPrefix = find ((`isPrefixOf` testFunction t) . generatorPrefix)

getGenerators :: [Test] -> [Generator]
getGenerators =
  map head .
  groupBy  ((==) `on` generatorPrefix) .
  sortOn generatorPrefix .
  map getGenerator

showSetup :: Test -> String -> String
showSetup t var = "  " ++ var ++ " <- " ++ setup ++ "\n"
  where setup = generatorSetup (getGenerator t) t

generators :: [Generator]
generators =
  [ quickCheckPropertyGenerator
  , hunitTestCaseGeneratorDeprecated
  , hunitTestCaseGenerator
  , hspecTestCaseGenerator
  , tastyTestGroupGenerator
  ]

quickCheckPropertyGenerator :: Generator
quickCheckPropertyGenerator = Generator
  { generatorPrefix = "prop_"
  , generatorImport = "import qualified Test.Tasty.QuickCheck as QC\n"
  , generatorClass  = ""
  , generatorSetup  = \t -> "pure $ QC.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

unitPrefixDeprecationMessage :: String
unitPrefixDeprecationMessage =
  error $ concat
    [ "\n\n"
    , "----------------------------------------------------------\n"
    , "DEPRECATION NOTICE: The `case_` prefix is deprecated.\n"
    , "Please use the `unit_` prefix instead.\n"
    , "Please see https://github.com/lwm/tasty-discover/issues/95.\n"
    , "----------------------------------------------------------\n"
    ]

-- DEPRECATED: Use `unit_` instead
hunitTestCaseGeneratorDeprecated :: Generator
hunitTestCaseGeneratorDeprecated = Generator
  { generatorPrefix = "case_"
  , generatorImport = unitPrefixDeprecationMessage
  , generatorClass  = unitPrefixDeprecationMessage
  , generatorSetup  = const unitPrefixDeprecationMessage
  }

hunitTestCaseGenerator :: Generator
hunitTestCaseGenerator = Generator
  { generatorPrefix = "unit_"
  , generatorImport = "import qualified Test.Tasty.HUnit as HU\n"
  , generatorClass  = concat
    [ "class TestCase a where testCase :: String -> a -> IO T.TestTree\n"
    , "instance TestCase (IO ())                      where testCase n = pure . HU.testCase      n\n"
    , "instance TestCase (IO String)                  where testCase n = pure . HU.testCaseInfo  n\n"
    , "instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . HU.testCaseSteps n\n"
    ]
  , generatorSetup  = \t -> "testCase \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

hspecTestCaseGenerator :: Generator
hspecTestCaseGenerator = Generator
  { generatorPrefix = "spec_"
  , generatorImport = "import qualified Test.Tasty.Hspec as HS\n"
  , generatorClass  = ""
  , generatorSetup  = \t -> "HS.testSpec \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

tastyTestGroupGenerator :: Generator
tastyTestGroupGenerator = Generator
  { generatorPrefix = "test_"
  , generatorImport = ""
  , generatorClass  = concat
    [ "class TestGroup a where testGroup :: String -> a -> IO T.TestTree\n"
    , "instance TestGroup T.TestTree        where testGroup _ a = pure a\n"
    , "instance TestGroup [T.TestTree]      where testGroup n a = pure $ T.testGroup n a\n"
    , "instance TestGroup (IO T.TestTree)   where testGroup _ a = a\n"
    , "instance TestGroup (IO [T.TestTree]) where testGroup n a = T.testGroup n <$> a\n"
    ]
  , generatorSetup  = \t -> "testGroup \"" ++ name t ++ "\" " ++ qualifyFunction t
  }

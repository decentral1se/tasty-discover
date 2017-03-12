module Test.Tasty.Type
  ( Test(..)
  , mkTest
  , Generator(..)
  , Config(..)
  ) where

import System.FilePath (pathSeparator, dropExtension)

data Test = Test
  { testModule   :: String
  , testFunction :: String
  } deriving Show

mkTest :: FilePath -> String -> Test
mkTest = Test . chooser pathSeparator '.' . dropExtension
  where chooser c1 c2 = map $ \c3 -> if c3 == c1 then c2 else c3

data Generator = Generator
  { generatorPrefix  :: String
  , generatorImport  :: String
  , generatorClass   :: String
  , generatorSetup   :: Test -> String
  }

instance Show Generator where
  show generator = concat
    [ generatorPrefix generator
    , generatorImport generator
    , generatorClass  generator
    , "<function:generatorSetup :: Test -> String>"
    ]

type Ingredient = String

data Config = Config
  { moduleSuffix        :: Maybe String
  , generatedModuleName :: Maybe String
  , ignoredModules      :: [FilePath]
  , tastyIngredients    :: [Ingredient]
  , noModuleSuffix      :: Bool
  , debug               :: Bool
  } deriving (Show)

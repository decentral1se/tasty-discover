-- | Types.

module Test.Tasty.Type where

-- | A test type. Corresponds to a test file path and module name.
data Test = Test {
  testFile   :: FilePath
, testModule :: String
} deriving (Eq, Show)

-- | A configuration type.
--   Constructor values are parsed from the preprocessor file.
data Config = Config {
  configModuleSuffix :: Maybe String
, noModuleSuffix     :: Bool
, ignoredModules     :: [FilePath]
} deriving (Eq, Show)


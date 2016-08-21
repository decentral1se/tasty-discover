-- | Types.

module Test.Tasty.Type where

-- | @TODO
data Test = Test {
  testFile   :: FilePath
, testModule :: String
} deriving (Eq, Show)

-- | @TODO
data Config = Config {
  configModuleSuffix :: Maybe String
} deriving (Eq, Show)


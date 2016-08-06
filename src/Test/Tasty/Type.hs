-- | Testing types.

module Test.Tasty.Type where

-- | @TODO
data Test = Test {
  testFile   :: FilePath
, testModule :: String
} deriving (Eq, Show)

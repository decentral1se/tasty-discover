-- | The configuration type and it's CLI options definition.

module Test.Tasty.Config (
    Config(Config)
  , defaultConfig
  , configModuleSuffix
  , options
) where

import           Test.Tasty.Prelude

data Config = Config {
  configModuleSuffix :: Maybe String
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config Nothing

options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["module-suffix"]
        (ReqArg (\s c -> c {configModuleSuffix = Just s}) "SUFFIX") ""
  ]
